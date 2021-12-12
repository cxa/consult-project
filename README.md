# consult-project

[Consult](https://github.com/minad/consult) integration for porject.

## Usage

### Install with [use-package](https://github.com/jwiegley/use-package)/[straight.el](https://github.com/raxod502/straight.el):

```elisp
(use-package consult-project
  :straight (consult-project :type git :host github :repo "cxa/consult-project")
  :custom (consult-project-recentf-max-projects 5)
  :bind ("M-p" . consult-project))
```

### or clone to your `load-path`.
