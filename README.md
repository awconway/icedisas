# Iowa satisfaction with anesthesia scale scores for cardiac implantable electronic device procedures performed with procedural sedation


>This repository hosts raw data and code required to completely reproduce the statistical analyses.  All code is in `R`. The [targets](https://github.com/ropensci/targets) package was used to manage the workflow. Run this line of code in the console to reproduce the analysis:

```
targets::tar_make()
```

You will see the targets being built by `targets`. `tar_load` will load the value of the target to the global environment. `tar_read` will read a target's return value from its file in _targets/objects/. For example, you can use `tar_read` to view plots. 

