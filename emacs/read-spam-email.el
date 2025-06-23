;;; -*- lexical-binding: t -*-

;;; Finally, I can browse to these:
;; auto-browse to https://mail.google.com/mail/u/0/#label/spam-todo
;; auto-browse to https://mail.google.com/mail/u/1/#label/spam-todo
;; auto-browse to https://mail.google.com/mail/u/0/#spam
;; auto-browse to https://mail.google.com/mail/u/1/#spam

(eval-when-compile
  (require 'mew)         ;; for macro (!) mew-summary-prepare-draft
  (require 'mew-summary) ;; for macro (!) mew-summary-prepare-draft
  )

(require 'mew-mde)

(autoload 'browse-url-once "browse-url-once")

(defun browse-spam (decile)
  "In web browser, browse up through given decile of spam."
  (interactive
   (list (read-number "Decile (1-10): ")))
  (dotimes (i (floor (* (/ decile 10.0) (length spam-mail-searches))))
    (browse-url-once (concat "https://mail.google.com/mail/u/0/#search/" (nth i spam-mail-searches)))
    (browse-url-once (concat "https://mail.google.com/mail/u/1/#search/" (nth i spam-mail-searches)))
    ))


;; TODO: defvar
(setq spam-mail-searches
      '(
        ;; Usually false positive
        "in:spam+ecoop-info"
        "in:spam+subject:seworld"
        "in:spam+subject:seajug"
        "in:spam+subject:typetools"
        "in:spam+lombok"
        "in:spam from:nytimes.com"
        "in:spam list:drool.mit.edu"

        ;; Specific senders and recipients; easy to triage, catches lots
        "in:spam from:cs.washington.edu"
        "in:spam to:mernst@rice.edu"
        "in:spam to:mernst@cs.rice.edu"
        "in:spam+EdScoop"
        "in:spam+FedScoop"
        "in:spam+Scoop+News"
        "in:spam+%22golf+wire%22"
        "in:spam+Imperva"
        "in:spam+labelbox"
        "in:spam+uw-security-research"
        "in:spam+Speechelo"
        "in:spam from:linkedin -from:linkedin.com"
        "in:spam from:linkedin -from:amazon.com"

        "in:spam+from:%22Anand%20Raghavan%22"
        "in:spam+from:%22Cooling%20Bra%20Pro%22"
        "in:spam+from:%22Google%20Calendar%22"
        "in:spam+from:%22Innovation%20News%20Network%22"
        "in:spam+from:%22My News Article%22"
        "in:spam+from:%22Ted's%20Wood%20Working%22"
        "in:spam+from:%22ZEISS%20Microscopy%22"
        "in:spam+from:EDIBON"
        "in:spam+from:GreenNewDealNetwork.org"
        "in:spam+from:McAfee"
        "in:spam+from:Norton"
        "in:spam+from:Plantronics"
        "in:spam+from:RingCentral"
        "in:spam+from:boojoy"
        "in:spam+from:boolex"
        "in:spam+from:dr"
        "in:spam+from:fortinet"
        "in:spam+from:isualum@mail.iastate.edu"
        "in:spam+from:life360"
        "in:spam+from:mr"
        "in:spam+from:mrs"
        "in:spam+from:ms"

        "in:spam+from:%22michael.ernst%22"
        "in:spam+from:mernst"
        "in:spam+from:michaelernst"

        "in:spam+subject:Micheal"
        "in:spam+subject:Michael"
        "in:spam+subject:Trump"
        "in:spam+subject:%22michael.ernst%22"
        "in:spam+subject:michaelernst"
        "in:spam+subject:invitation"
        "in:spam+subject:mernst"
        "in:spam+subject:security+subject:alert+subject:for"

        "in:spam+angi"
        "in:spam+bitcoin"
        "in:spam+Casino"
        "in:spam+CBD"
        "in:spam+coronavirus"
        "in:spam+covid"
        "in:spam+cyberweek"
        "in:spam+democratic"
        "in:spam+Fluke"
        "in:spam+fuckbuddy"
        "in:spam+hookup"
        "in:spam+indonesia"
        "in:spam+keto"
        "in:spam+ketosis"
        "in:spam+KN95"
        "in:spam+Micheal"
        "in:spam+nick+hellen"
        "in:spam+Pluralsight"
        "in:spam+republican"
        "in:spam+Seattle"
        "in:spam+Syxsense"
        "in:spam+TeaParty.org"
        "in:spam+techwatch"
        "in:spam+TechWatch.org"
        "in:spam+temperature"
        "in:spam+thermometer"
        ))
