;;; -*- lexical-binding: t -*-

;; 
;; # This sequence of two commands works:
;; wget --keep-session-cookies --save-cookies ~/cookies-saved.txt --post-data 'email=mernst@cs.washington.edu&password=rzRI8bl7tfVD11oe' https://pldi16.hotcrp.com/
;; 
;; # Get a list of all papers by doing:
;; wget --keep-session-cookies --load-cookies ~/cookies-saved.txt 'https://pldi16.hotcrp.com/search?q=&t=s' -O all-submissions.html
;; # and find the line containing "needload"
;; 
;; # Download a single paper with
;; wget --keep-session-cookies --load-cookies ~/cookies-saved.txt 'https://pldi16.hotcrp.com/paper/2' -p -k -H
;; 
;; # Postprocessing, after downloading everything:
;; 
;; I need to remove:
;; ?mtime=1452886024" crossorigin="anonymous"
;; 
;; and also rename files.
;; 
;; And also remove any line that matches
;;   https://pldi16.hotcrp.com/review/[0-9]+[A-Z]
;; to minimize risk of clicking on it.




;; Running the Lisp form creates a list of shell commands that can be pasted into a command line.

;; For CyberChair Pro:
;;  * Obtain the list of papers from the bidding page and then do:
;;    (progn (replace-string "*" "") (delete-non-matching-lines "[0-9]"))
;;    If I only have access to my papers because it's too early in the process, then click on "DL/Review Summ./Assigned (s)"
;;  * Obtain the cookies by opening any "online discussion" link

;; PLDI 2016
(let* ((all-papers
	'(
	  2 3 4 5 8 11 12 15 19 20 21 22 23 25 27 28 29 30 31 33
	  34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 52
	  53 55 56 57 58 59 60 61 62 63 64 65 67 68 69 70 72 73
	  74 75 76 77 78 79 80 81 82 83 84 85 86 88 89 90 91 92
	  93 94 95 96 97 98 99 100 101 102 103 105 106 107 108
	  109 111 112 113 114 115 116 117 118 119 121 122 123 124
	  125 126 127 128 129 130 131 132 133 134 135 136 137 138
	  139 140 141 142 143 144 145 146 147 148 149 150 151 152
	  154 155 156 158 159 160 161 162 163 165 166 167 168 169
	  170 171 173 174 175 176 177 179 180 181 182 183 184 185
	  186 187 188 189 190 191 192 194 195 196 197 198 199 200
	  201 202 203 204 205 206 207 208 209 210 211 212 213 214
	  215 216 217 218 220 221 222 223 224 225 226 227 228 229
	  230 231 232 233 234 235 236 237 238 239 240 241 242 243
	  244 245 246 247 248 249 250 251 252 253 254 255 256 257
	  258 259 260 261 262 263 265 267 268 269 270 271 273 274
	  275 276 277 278 279 280 281 282 283 284 285 286 288 289
	  290 291 292 293 294 295 296 297 298 299 300 302 303 304
	  305 306 307 308 309 310 311 312 313 314 317 318 319 321
	  322 323 325
	  ))
      (my-papers '(62 74 83 161 166 202 209 228 248 302))
      ;; change this line to choose all-papers or my-papers
      (papers (shuffle all-papers)))
  (while papers
    (let ((paper (car papers)))
      ;; All the reviews
      (insert (format "wget --keep-session-cookies --load-cookies ~/cookies-saved.txt 'https://pldi16.hotcrp.com/paper/%d' -p -k -H\n" paper))
      (insert (format "sleep %d\n" (random 200))))
    (setq papers (cdr papers))))
