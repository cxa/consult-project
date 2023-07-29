> [!WARNING]  
> Use `consult-project-buffer` instead.

# consult-project

[Consult](https://github.com/minad/consult) integration for Emacs builtin [project](https://elpa.gnu.org/packages/project.html) manager.

## Config example

### Install with [use-package](https://github.com/jwiegley/use-package) and [quelpa](https://github.com/quelpa/quelpa):

```elisp
(use-package consult-project
  :quelpa (consult-project :fetcher github :repo "cxa/consult-project")
  :bind ("C-x C-p" . consult-project))
```

### or clone to your `load-path` and manually `require` to load.
