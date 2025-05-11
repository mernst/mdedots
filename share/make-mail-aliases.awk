#! /usr/bin/gawk -f
# /usr/local/bin/gawk -f

# Build mail-aliases file from address file.

# Use this way:
#   make-mail-aliases.awk < ~/private/addresses.tex > ~/.mailrc.el

# Format is:
# email: [alias =] result
# If there's no alias, use firstname.lastname
# [If there is an alias, use that also anyway?]
# All aliases are lowercase.  The advantage of NOT doing this is to
# make two separate lists in completing-read; but I think that more
# confusing than helpful.

# Does not work correctly on aliases with names in quotes, eg
# "G. Del Merritt" <mit-eddie!gatech!itcatl!giant!del>, because this
# gets surrounded by quotes and then the wrong quotes are matched by Emacs.

BEGIN { # # print "(emacs-18 (require 'mail-abbrevs))"
	# # print "(emacs-19 (require 'mailabbrev))"
        # print "(require 'mailabbrev)"
        print ";; This file is created by make-mail-aliases.awk,"
        print ";;   which is called by make-mail-aliases."
	print "(emacs-fsf (require 'mailabbrev))"
	print "(xemacs (require 'mail-abbrevs))"
	# print ";; used by Emacs 19 only, but indicates this file has been read"
	print ";; indicates this file has been read"
	print "(define-abbrev-table 'mail-abbrevs '())" }

/^email:/ { if (!($1 == "email:"))
		  print ";; Problem with line:  " $0
		else
		  { if ($3 == "=")
		      { address = substr($0, index($0, "=") + 1)
		        # remove leading spaces
		        sub(/^ +/, "", address)
		        alias = $2 }
		    else
		      { address = substr($0, 7)
		        # remove leading spaces
		        sub(/^ +/, "", address)
		        alias = substr(address, 1, match(address, /[ \t]*</) - 1)
		        gsub(/ /, ".", alias) }
		    # escape all double-quote marks
		    gsub(/"/, "\\\"", address)
		    if ((alias != "") && (alias != address))
		      { print "(mde-mail-alias \"" tolower(alias) "\" \"" address "\")" } } }

END { print "(setq mail-abbrev-aliases-need-to-be-resolved t)"
      print "(mail-resolve-all-aliases)" }
