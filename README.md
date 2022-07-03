# consult-project

[Consult](https://github.com/minad/consult) integration for project.

## Config example

### Install with [use-package](https://github.com/jwiegley/use-package) and [quelpa](https://github.com/quelpa/quelpa):

```elisp
(use-package consult-project
  :quelpa (consult-project :fetcher github :repo "cxa/consult-project")
  :custom (consult-project-recentf-max-projects 5)
  :bind ("C-x C-p" . consult-project))
```

### or clone to your `load-path`.
