(setq package-native-compile t)
(setq native-comp-deferred-compilation t)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))
