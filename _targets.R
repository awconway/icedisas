library(targets) # devtools::install_github("ropensci/targets")


tar_option_set(
  packages = c(
    "tidyverse",
    "icedisas", # devtools::install_github("awconway/icedisas")
    "gtsummary",
    "gt"
  )
)

list(
  tar_target(
    data_raw,
    read_csv("https://raw.githubusercontent.com/awconway/hfnosedrct/6b6374222d8e413cfe05448d0fb8adb675ea0849/data/HighFlowNasalOxygenT_DATA_2020-05-21_1511.csv")
  ),
  tar_target(data_label, label_data(data_raw) %>%
    filter(str_detect(id, "P")) %>%
    select(everything(), -starts_with("screen")) %>%
    # P084 removed (procedure was not performed)
    filter(id != "P084")),
  tar_target(
    data,
    data_label %>%
      rowwise() %>%
      mutate(isas_mean = mean(c(
        isasvomit,
        isassameanesthetic,
        isasitch,
        isasrelaxed,
        isaspain,
        isassafe,
        isastoocoldhot,
        isassurgerypain,
        isassatisfiedcare,
        isasfeltgood,
        isashurt
      ), na.rm = TRUE)) %>%
      ungroup() %>%
      select(
        id,
        age,
        sex.factor,
        lastfood,
        lastfluids,
        procedurestart,
        procedureend,
        propofol,
        midazolam,
        fentanyl,
        remifentanil,
        otheropioidname,
        otheropioiddose,
        otheropioidunits,
        admward.factor,
        admsource.factor,
        asaclass.factor,
        procedure.factor,
        deepsedation.factor,
        isas_mean,
        starts_with("isas"),
      )
  ),
  tar_target(
    isas_plot,
    data %>%
      mutate(ISAS = "ISAS") %>%
      ggplot() +
      ggbeeswarm::geom_beeswarm(aes(y = isas_mean, x = ISAS)) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        legend.position = "none"
      ) +
      labs(y = "ISAS score")
  ),
  tar_target(model_data, data %>%
    mutate(
      procedure.factor = fct_drop(procedure.factor),
      admward.factor = fct_drop(admward.factor),
      sex.factor = fct_drop(sex.factor),
      procedure.factor = case_when(
        procedure.factor == "PPM" ~ "PPM",
        procedure.factor == "PPM generator change" ~ "PPM",
        procedure.factor == "PPM lead revision" ~ "PPM",
        procedure.factor == "ICD" ~ "ICD",
        procedure.factor == "ICD lead revision" ~ "ICD",
        procedure.factor == "ICD generator change" ~ "ICD",
        procedure.factor == "CRT-D" ~ "CRT",
        procedure.factor == "CRT-P" ~ "CRT",
        TRUE ~ "Other"
      ),
      `Cardiac Resynchronization Therapy` = ifelse(
        procedure.factor == "CRT", 1, 0
      ),
      other_sedation = ifelse(is.na(otheropioiddose), 1, 0),
      `Total dose of fentanyl (mcg)` = ifelse(
        !is.na(remifentanil), remifentanil, fentanyl
      ),
      `Procedure duration (hours)` =
        as.numeric(procedureend - procedurestart) / 60,
      food_fast = procedurestart - lastfood,
      fluid_fast = procedurestart - lastfluids,
      `Day case` = ifelse(admward.factor == "Day surgery", 1, 0)
    ) %>%
    replace_na(list(
      propofol = 0,
      midazolam = 0,
      fentanyl = 0
    )) %>%
    rename(`ASA Class` = "asaclass.factor")),
  tar_target(model, lm(isas_mean ~
  midazolam +
    propofol +
    `Total dose of fentanyl (mcg)` +
    age +
    sex.factor +
    `ASA Class` +
    `Day case` +
    `Cardiac Resynchronization Therapy` +
    `Procedure duration (hours)`,
  data = model_data
  )),
  tar_target(model_table, model %>%
    tbl_regression() %>%
    bold_p(t = 0.05))
)