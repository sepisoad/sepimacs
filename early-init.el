(setq
 package-enable-at-startup nil
 gc-cons-threshold most-positive-fixnum
 read-process-output-max (* 1024 1024)
 redisplay-dont-pause t
 gc-cons-percentage 0.6
 frame-resize-pixelwise t
 inhibit-compacting-font-caches t
 frame-inhibit-implied-resize t
 frame-resize-pixelwise t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(provide 'early-init)

;;; early-init.el ends here
