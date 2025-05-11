;;; -*- lexical-binding: t -*-

;; This file exists so that I can do   (load "tiny-screen")  when necessary.

(set-frame-size (selected-frame) 79 23)
;; Consider something like
;; (setq baud-rate 57600)
;; because Emacs may mis-guess the baud rate and throttle sending characters.
