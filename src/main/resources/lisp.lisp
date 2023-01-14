(set 'setq (syntax (name value) (list (quote set) (list (quote quote) name) value)))
(setq define-lambda (syntax (name pars body) (list (quote setq) name (list (quote lambda) pars body))))
(setq define-syntax (syntax (name pars body) (list (quote setq) name (list (quote syntax) pars body))))
