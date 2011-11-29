;;
;; nagios-mode, an Emacs mode for Nagios <http://www.nagios.org/>
;; configuration files.
;;
;; Copyright Michael Orlitzky
;;
;; http://michael.orlitzky.com/
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; http://www.fsf.org/licensing/licenses/gpl.html
;;

(require 'font-lock)
(require 'regexp-opt)


;; Custom Variables

(defcustom nagios-indent-level 2
  "Number of spaces in one indentation (tab)."
  :type 'integer :group 'nagios
  )



;; Indentation Voodoo

(defun nagios-indent-line(&optional flag)
  "Indents a line, taking nesting into account."
  (interactive)
  (nagios-indent-to (nagios-calculate-indent))
  )


(defun nagios-beginning-of-line-pos()
  ;; Return the point position corresponding to the beginning
  ;; of the current line.
  (save-excursion
    (beginning-of-line)
    (point)
  )
)

(defun nagios-end-of-line-pos()
  ;; Return the point position corresponding to the end
  ;; of the current line.
  (save-excursion
    (end-of-line)
    (point)
  )
)

(defun nagios-point-offset()
  ;; How far are we from the beginning of the line?
  (- (point) (nagios-beginning-of-line-pos))
)

(defun nagios-first-char-offset()
  ;; How far is the first character on this line
  ;; from the beginning of the line?
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
  )
)

(defun nagios-first-char-pos()
  ;; What's the position of the first character on this line?
  (+ (nagios-beginning-of-line-pos) (nagios-first-char-offset))
)

(defun nagios-indent-to(indent-column)
  "Indent the current line to column indent-column."
  ;; Store the point in orig-pos.
  (let ((orig-point (point)))

    ;; And store the offset of the first character (with respect to the
    ;; beginning of the line) in orig-first-char-offset.
    (let ((orig-first-char-offset (nagios-first-char-offset)))

      ;; Delete any leading whitespace, and move the point to the
      ;; beginning of the line.
      (delete-region (nagios-beginning-of-line-pos) (nagios-first-char-pos))
      (beginning-of-line)

      ;; Now insert indent-column spaces.
      (let ((indent-remaining indent-column))
	(while (< 0 indent-remaining)
	  (insert " ")
	  (setq indent-remaining (- indent-remaining 1)))
      )

      ;; The text on the current line just moved left/right some amount;
      ;; call it text-delta. We want to move the point that same distance.
      (let ((text-delta (- (nagios-first-char-offset) orig-first-char-offset)))
	(goto-char (+ orig-point text-delta))
      )

      ;; The point should never wind up to the left of indent-column, so
      ;; if it's there, move it over to indent-column.
      (if (< (nagios-point-offset) indent-column)
	  (goto-char (+ (nagios-beginning-of-line-pos) indent-column))
      )
    )
  )
)


(defun nagios-char-is-commented(pos)
  "True if the character at position pos is commented, nil otherwise."
  (save-excursion
    (goto-char pos)
    (re-search-backward "\\(#\\|;\\)" (nagios-beginning-of-line-pos) t)
  )
)

(defun nagios-char-is-commented-and-valid(pos)
  "True if the character at position pos is commented and non-nil.
   Nil otherwise."
  (if (eq nil pos)
      nil
      (nagios-char-is-commented pos)
  )
)


(defun nagios-last-opening-brace()
  "Returns the position of the last opening brace, with
   respect to the current point. Ignores braces which
   are commented out."
  (save-excursion
    (let ((lob (re-search-backward "{" nil t)))

      (while (nagios-char-is-commented-and-valid lob)
	(goto-char lob)
	(setq lob (re-search-backward "{" nil t))
      )

      (if lob
	  lob
	  -1)
    )
  )
)


(defun nagios-last-closing-brace()
  "Get the position of the last closing brace, with
   respect to the current point. Ignores braces which
   are commented out."
  (save-excursion
    (let ((lcb (re-search-backward "}" nil t)))

      (while (nagios-char-is-commented-and-valid lcb)
	(goto-char lcb)
	(setq lcb (re-search-backward "}" nil t))
      )

      (if lcb
	  lcb
	  -1)
    )
  )
)

(defun nagios-in-block()
  "Determine if the point is inside of a {} block."

  ;; If the last brace seen in the buffer is an opening brace, we're
  ;; in a block. Otherwise, we aren't.
  (if (>= (nagios-last-closing-brace) (nagios-last-opening-brace))
      nil
      t)
)


(defun nagios-brace-on-line()
  ;; Is there a curly brace on this line?
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[{}]" (nagios-end-of-line-pos) t)
  )
)


(defun nagios-calculate-indent()
  "Calculate the level of indentation."

  ;; We're either inside a block, or we aren't.
  ;; Initialize the indent variable to either nagios-indent-level
  ;; or 0 depending on whether or not we're in a block.
  (let ((indent (if (nagios-in-block)
		    nagios-indent-level
		    0)
	)
       )

    ;; Set the indentation level to 0 if we find either brace on this
    ;; line and.
    (if (and (nagios-brace-on-line)
	     (not (nagios-char-is-commented (nagios-brace-on-line))))
	0
        indent
    )
  )
)


;; Keymaps

(defun nagios-insert-right-brace-and-indent()
  "Insert a '}' character, and indent the line."
  (interactive)
  (insert "}")
  (nagios-indent-line)
  )


(defvar nagios-mode-map()
  "Keymap used in nagios mode.")

(when (not nagios-mode-map)
  (setq nagios-mode-map (make-sparse-keymap))
  (define-key nagios-mode-map
    (read-kbd-macro "}")
    'nagios-insert-right-brace-and-indent)
  )



(defconst nagios-directives
  (eval-when-compile
    (concat "^[ \t\r\n]*"

    (regexp-opt
     '("active_checks_enabled" "address" "alias" "check_command"
       "check_freshness" "check_interval" "check_period" "checks_enabled"
       "command_line" "command_name" "contactgroups" "contact_groups"
       "contactgroup_members" "contact_name" "contactgroup_name" "contacts"
       "dependent_host_name" "dependent_service_description" "email"
       "event_handler" "event_handler_enabled" "execution_failure_criteria"
       "failure_prediction_enabled" "first_notification"
       "first_notification_delay" "flap_detection_enabled" "freshness_threshold"
       "friday" "high_flap_threshold" "host_name" "host_notification_commands"
       "host_notification_options" "host_notification_period"
       "host_notifications_enabled" "hostgroup_name" "hostgroups"
       "is_volatile" "last_notification" "low_flap_threshold"
       "max_check_attempts" "members" "monday" "normal_check_interval"
       "notes" "notification_failure_criteria"
       "notification_interval" "notification_options"
       "notification_period" "notifications_enabled"
       "obsess_over_service" "pager" "parallelize_check"
       "parents" "passive_checks_enabled"
       "process_perf_data" "retain_nonstatus_information"
       "retain_status_information" "retry_check_interval"
       "retry_interval" "saturday" "service_description"
       "service_notification_commands" "service_notification_options"
       "service_notification_period" "service_notifications_enabled"
       "servicegroup_name" "stalking_options"
       "sunday" "thursday" "timeperiod_name" "tuesday" "wednesday") t)

    "[ \r\n\t]+")
    )
  )



(defconst nagios-macros
  (eval-when-compile
    (regexp-opt
     '("$ADMINEMAIL$"
       "$ADMINPAGER$"
       "$ARG1$"
       "$ARG10$"
       "$ARG11$"
       "$ARG12$"
       "$ARG13$"
       "$ARG14$"
       "$ARG15$"
       "$ARG16$"
       "$ARG17$"
       "$ARG18$"
       "$ARG19$"
       "$ARG2$"
       "$ARG20$"
       "$ARG21$"
       "$ARG22$"
       "$ARG23$"
       "$ARG24$"
       "$ARG25$"
       "$ARG26$"
       "$ARG27$"
       "$ARG28$"
       "$ARG29$"
       "$ARG3$"
       "$ARG30$"
       "$ARG31$"
       "$ARG32$"
       "$ARG4$"
       "$ARG5$"
       "$ARG6$"
       "$ARG7$"
       "$ARG8$"
       "$ARG9$"
       "$COMMANDFILE$"
       "$CONTACTALIAS$"
       "$CONTACTEMAIL$"
       "$CONTACTGROUPALIAS$"
       "$CONTACTGROUPMEMBERS$"
       "$CONTACTGROUPNAME$"
       "$CONTACTGROUPNAMES$"
       "$CONTACTNAME$"
       "$CONTACTPAGER$"
       "$DATE$"
       "$EVENTSTARTTIME$"
       "$HOSTACKAUTHOR$"
       "$HOSTACKAUTHORALIAS$"
       "$HOSTACKAUTHORNAME$"
       "$HOSTACKCOMMENT$"
       "$HOSTACTIONURL$"
       "$HOSTADDRESS$"
       "$HOSTALIAS$"
       "$HOSTATTEMPT$"
       "$HOSTCHECKCOMMAND$"
       "$HOSTCHECKTYPE$"
       "$HOSTDISPLAYNAME$"
       "$HOSTDOWNTIME$"
       "$HOSTDURATION$"
       "$HOSTDURATIONSEC$"
       "$HOSTEVENTID$"
       "$HOSTEXECUTIONTIME$"
       "$HOSTGROUPACTIONURL$"
       "$HOSTGROUPALIAS$"
       "$HOSTGROUPMEMBERS$"
       "$HOSTGROUPNAME$"
       "$HOSTGROUPNAMES$"
       "$HOSTGROUPNOTES$"
       "$HOSTGROUPNOTESURL$"
       "$HOSTLATENCY$"
       "$HOSTNAME$"
       "$HOSTNOTES$"
       "$HOSTNOTESURL$"
       "$HOSTNOTIFICATIONID$"
       "$HOSTNOTIFICATIONNUMBER$"
       "$HOSTOUTPUT$"
       "$HOSTPERCENTCHANGE$"
       "$HOSTPERFDATA$"
       "$HOSTPERFDATAFILE$"
       "$HOSTPROBLEMID$"
       "$HOSTSTATE$"
       "$HOSTSTATEID$"
       "$HOSTSTATETYPE$"
       "$ISVALIDTIME$"
       "$LASTHOSTCHECK$"
       "$LASTHOSTDOWN$"
       "$LASTHOSTEVENTID$"
       "$LASTHOSTPROBLEMID$"
       "$LASTHOSTSTATE$"
       "$LASTHOSTSTATECHANGE$"
       "$LASTHOSTSTATEID$"
       "$LASTHOSTUNREACHABLE$"
       "$LASTHOSTUP$"
       "$LASTSERVICECHECK$"
       "$LASTSERVICECRITICAL$"
       "$LASTSERVICEEVENTID$"
       "$LASTSERVICEOK$"
       "$LASTSERVICEPROBLEMID$"
       "$LASTSERVICESTATE$"
       "$LASTSERVICESTATECHANGE$"
       "$LASTSERVICESTATEID$"
       "$LASTSERVICEUNKNOWN$"
       "$LASTSERVICEWARNING$"
       "$LOGFILE$"
       "$LONGDATETIME$"
       "$LONGHOSTOUTPUT$"
       "$LONGSERVICEOUTPUT$"
       "$MAINCONFIGFILE$"
       "$MAXHOSTATTEMPTS$"
       "$MAXSERVICEATTEMPTS$"
       "$NEXTVALIDTIME$"
       "$NOTIFICATIONAUTHOR$"
       "$NOTIFICATIONAUTHORALIAS$"
       "$NOTIFICATIONAUTHORNAME$"
       "$NOTIFICATIONCOMMENT$"
       "$NOTIFICATIONISESCALATED$"
       "$NOTIFICATIONNUMBER$"
       "$NOTIFICATIONRECIPIENTS$"
       "$NOTIFICATIONTYPE$"
       "$OBJECTCACHEFILE$"
       "$PROCESSSTARTTIME$"
       "$RESOURCEFILE$"
       "$RETENTIONDATAFILE$"
       "$SERVICEACKAUTHOR$"
       "$SERVICEACKAUTHORALIAS$"
       "$SERVICEACKAUTHORNAME$"
       "$SERVICEACKCOMMENT$"
       "$SERVICEACTIONURL$"
       "$SERVICEATTEMPT$"
       "$SERVICECHECKCOMMAND$"
       "$SERVICECHECKTYPE$"
       "$SERVICEDESC$"
       "$SERVICEDISPLAYNAME$"
       "$SERVICEDOWNTIME$"
       "$SERVICEDURATION$"
       "$SERVICEDURATIONSEC$"
       "$SERVICEEVENTID$"
       "$SERVICEEXECUTIONTIME$"
       "$SERVICEGROUPACTIONURL$"
       "$SERVICEGROUPALIAS$"
       "$SERVICEGROUPMEMBERS$"
       "$SERVICEGROUPNAME$"
       "$SERVICEGROUPNAMES$"
       "$SERVICEGROUPNOTES$"
       "$SERVICEGROUPNOTESURL$"
       "$SERVICEISVOLATILE$"
       "$SERVICELATENCY$"
       "$SERVICENOTES$"
       "$SERVICENOTESURL$"
       "$SERVICENOTIFICATIONID$"
       "$SERVICENOTIFICATIONNUMBER$"
       "$SERVICEOUTPUT$"
       "$SERVICEPERCENTCHANGE$"
       "$SERVICEPERFDATA$"
       "$SERVICEPERFDATAFILE$"
       "$SERVICEPROBLEMID$"
       "$SERVICESTATE$"
       "$SERVICESTATEID$"
       "$SERVICESTATETYPE$"
       "$SHORTDATETIME$"
       "$STATUSDATAFILE$"
       "$TEMPFILE$"
       "$TEMPPATH$"
       "$TIME$"
       "$TIMET$"
       "$TOTALHOSTPROBLEMS$"
       "$TOTALHOSTPROBLEMSUNHANDLED$"
       "$TOTALHOSTSDOWN$"
       "$TOTALHOSTSDOWNUNHANDLED$"
       "$TOTALHOSTSERVICES$"
       "$TOTALHOSTSERVICESCRITICAL$"
       "$TOTALHOSTSERVICESOK$"
       "$TOTALHOSTSERVICESUNKNOWN$"
       "$TOTALHOSTSERVICESWARNING$"
       "$TOTALHOSTSUNREACHABLE$"
       "$TOTALHOSTSUNREACHABLEUNHANDLED$"
       "$TOTALHOSTSUP$"
       "$TOTALSERVICEPROBLEMS$"
       "$TOTALSERVICEPROBLEMSUNHANDLED$"
       "$TOTALSERVICESCRITICAL$"
       "$TOTALSERVICESCRITICALUNHANDLED$"
       "$TOTALSERVICESOK$"
       "$TOTALSERVICESUNKNOWN$"
       "$TOTALSERVICESUNKNOWNUNHANDLED$"
       "$TOTALSERVICESWARNING$"
       "$TOTALSERVICESWARNINGUNHANDLED$"
       "$USER1$"
       "$USER10$"
       "$USER100$"
       "$USER101$"
       "$USER102$"
       "$USER103$"
       "$USER104$"
       "$USER105$"
       "$USER106$"
       "$USER107$"
       "$USER108$"
       "$USER109$"
       "$USER11$"
       "$USER110$"
       "$USER111$"
       "$USER112$"
       "$USER113$"
       "$USER114$"
       "$USER115$"
       "$USER116$"
       "$USER117$"
       "$USER118$"
       "$USER119$"
       "$USER12$"
       "$USER120$"
       "$USER121$"
       "$USER122$"
       "$USER123$"
       "$USER124$"
       "$USER125$"
       "$USER126$"
       "$USER127$"
       "$USER128$"
       "$USER129$"
       "$USER13$"
       "$USER130$"
       "$USER131$"
       "$USER132$"
       "$USER133$"
       "$USER134$"
       "$USER135$"
       "$USER136$"
       "$USER137$"
       "$USER138$"
       "$USER139$"
       "$USER14$"
       "$USER140$"
       "$USER141$"
       "$USER142$"
       "$USER143$"
       "$USER144$"
       "$USER145$"
       "$USER146$"
       "$USER147$"
       "$USER148$"
       "$USER149$"
       "$USER15$"
       "$USER150$"
       "$USER151$"
       "$USER152$"
       "$USER153$"
       "$USER154$"
       "$USER155$"
       "$USER156$"
       "$USER157$"
       "$USER158$"
       "$USER159$"
       "$USER16$"
       "$USER160$"
       "$USER161$"
       "$USER162$"
       "$USER163$"
       "$USER164$"
       "$USER165$"
       "$USER166$"
       "$USER167$"
       "$USER168$"
       "$USER169$"
       "$USER17$"
       "$USER170$"
       "$USER171$"
       "$USER172$"
       "$USER173$"
       "$USER174$"
       "$USER175$"
       "$USER176$"
       "$USER177$"
       "$USER178$"
       "$USER179$"
       "$USER18$"
       "$USER180$"
       "$USER181$"
       "$USER182$"
       "$USER183$"
       "$USER184$"
       "$USER185$"
       "$USER186$"
       "$USER187$"
       "$USER188$"
       "$USER189$"
       "$USER19$"
       "$USER190$"
       "$USER191$"
       "$USER192$"
       "$USER193$"
       "$USER194$"
       "$USER195$"
       "$USER196$"
       "$USER197$"
       "$USER198$"
       "$USER199$"
       "$USER2$"
       "$USER20$"
       "$USER200$"
       "$USER201$"
       "$USER202$"
       "$USER203$"
       "$USER204$"
       "$USER205$"
       "$USER206$"
       "$USER207$"
       "$USER208$"
       "$USER209$"
       "$USER21$"
       "$USER210$"
       "$USER211$"
       "$USER212$"
       "$USER213$"
       "$USER214$"
       "$USER215$"
       "$USER216$"
       "$USER217$"
       "$USER218$"
       "$USER219$"
       "$USER22$"
       "$USER220$"
       "$USER221$"
       "$USER222$"
       "$USER223$"
       "$USER224$"
       "$USER225$"
       "$USER226$"
       "$USER227$"
       "$USER228$"
       "$USER229$"
       "$USER23$"
       "$USER230$"
       "$USER231$"
       "$USER232$"
       "$USER233$"
       "$USER234$"
       "$USER235$"
       "$USER236$"
       "$USER237$"
       "$USER238$"
       "$USER239$"
       "$USER24$"
       "$USER240$"
       "$USER241$"
       "$USER242$"
       "$USER243$"
       "$USER244$"
       "$USER245$"
       "$USER246$"
       "$USER247$"
       "$USER248$"
       "$USER249$"
       "$USER25$"
       "$USER250$"
       "$USER251$"
       "$USER252$"
       "$USER253$"
       "$USER254$"
       "$USER255$"
       "$USER256$"
       "$USER26$"
       "$USER27$"
       "$USER28$"
       "$USER29$"
       "$USER3$"
       "$USER30$"
       "$USER31$"
       "$USER32$"
       "$USER33$"
       "$USER34$"
       "$USER35$"
       "$USER36$"
       "$USER37$"
       "$USER38$"
       "$USER39$"
       "$USER4$"
       "$USER40$"
       "$USER41$"
       "$USER42$"
       "$USER43$"
       "$USER44$"
       "$USER45$"
       "$USER46$"
       "$USER47$"
       "$USER48$"
       "$USER49$"
       "$USER5$"
       "$USER50$"
       "$USER51$"
       "$USER52$"
       "$USER53$"
       "$USER54$"
       "$USER55$"
       "$USER56$"
       "$USER57$"
       "$USER58$"
       "$USER59$"
       "$USER6$"
       "$USER60$"
       "$USER61$"
       "$USER62$"
       "$USER63$"
       "$USER64$"
       "$USER65$"
       "$USER66$"
       "$USER67$"
       "$USER68$"
       "$USER69$"
       "$USER7$"
       "$USER70$"
       "$USER71$"
       "$USER72$"
       "$USER73$"
       "$USER74$"
       "$USER75$"
       "$USER76$"
       "$USER77$"
       "$USER78$"
       "$USER79$"
       "$USER8$"
       "$USER80$"
       "$USER81$"
       "$USER82$"
       "$USER83$"
       "$USER84$"
       "$USER85$"
       "$USER86$"
       "$USER87$"
       "$USER88$"
       "$USER89$"
       "$USER9$"
       "$USER90$"
       "$USER91$"
       "$USER92$"
       "$USER93$"
       "$USER94$"
       "$USER95$"
       "$USER96$"
       "$USER97$"
       "$USER98$"
       "$USER99$")))
  )



(defconst nagios-definitions
  (eval-when-compile

    (concat "^[ \t\r\n]*"

	    "\\(" ;; Stick parenthesis around whatever comes out
	          ;; of regexp-opt. We use this to match a
	          ;; subexpression during font-lock.
	    (regexp-opt
	     '("define command"
	       "define contact"
	       "define contactgroup"
	       "define host"
	       "define hostdependency"
	       "define hostescalation"
	       "define hostextinfo"
	       "define hostgroup"
	       "define hostgroupescalation"
	       "define null"
	       "define service"
	       "define servicedependency"
	       "define serviceescalation"
	       "define serviceextinfo"
	       "define servicegroup"
	       "define timeperiod"))
	          ;; This closes the parentheses that we opened
	    "\\)" ;; before regexp-opt.

	    ;; These can be "terminated" by either an opening curly
	    ;; brace, or a space.
	    "\\({\\| \\)")
    )
  )



(defconst nagios-special
  (eval-when-compile
    (concat "^[ \t\r\n]*"

    (regexp-opt
     '("name" "register" "use") t)

    "[ \r\n\t]+"))
  )



;; The One True Font Locking Variable

(defvar nagios-font-lock-keywords
  (list
   (cons nagios-special font-lock-keyword-face)
   (cons nagios-directives font-lock-variable-name-face)
   (cons nagios-macros font-lock-constant-face)
   (cons nagios-definitions '(1 font-lock-function-name-face)))

  "Rules for highlighting Nagios configuration files."
  )



(defvar nagios-mode-syntax-table nil
  "Syntax table used in nagios-mode buffers.")
(if nagios-mode-syntax-table
    nil
  (setq nagios-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?# "< b" nagios-mode-syntax-table)  ;; Comment style 1
  (modify-syntax-entry ?\; "< b" nagios-mode-syntax-table) ;; Comment style 2
  (modify-syntax-entry ?\n "> b" nagios-mode-syntax-table) ;; End comment
  )


;; Main Mode Function

(defun nagios-mode()
  "Major mode for editing Nagios configuration files."

  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'syntax-begin-function)

  (set-syntax-table nagios-mode-syntax-table)

  (setq mode-name             "nagios"
	major-mode            'nagios-mode
	indent-line-function  'nagios-indent-line
	font-lock-defaults    '(nagios-font-lock-keywords)
	comment-start         "#"
	comment-start-skip    "#\|; +"
	comment-end           ""

	;; Since comments and strings do not span multiple lines,
	;; the syntax parser can safely start parsing at the beginning
	;; of any line.
	syntax-begin-function 'beginning-of-line
	)

  ;; Keyboard Mapping
  (use-local-map nagios-mode-map)

  ;; I don't /think/ I need to define this before attempting
  ;; to run it. Users can define it if they want.
  (run-hooks 'nagios-mode-hook)
  )


(provide 'nagios-mode)
