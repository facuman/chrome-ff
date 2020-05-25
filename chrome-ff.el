;;; chrome-ff.el --- Firefox remote tab control -*- lexical-binding: t; -*-

;; Copyright (C) 2020 facudeguzman@gmail.com
;;               
;;
;; All rights reserved

;; Version: 0.1
;; Author: Facundo de Guzman <facudeguzman@gmail>
;;
;; Maintainer: Facundo de Guzman <facudeguzman@gmail>
;; URL: https://github.com/facuman/chrome-ff.el
;; Package-Requires: ((emacs "25.1"))
;; Keywords: comm

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;
;;   * Redistributions in binary form must reproduce the above
;;     copyright notice, this list of conditions and the following
;;     disclaimer in the documentation and/or other materials
;;     provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;; This module requires chrome.el to work.
;;
;; Firefox needs to open the debug server port the communication to happen:
;;
;; firefox --safe-mode --start-debugger-server
;;
;; Also the following values should be set in a 'about:config' tab:
;; 	devtools.debugger.remote-enabled = true
;;	devtools.chrome.enabled = true
;;
;; Note: This changes may require a browser restart
;;

;;; Code:


(require 'chrome)

;; (setq chrome-ff--debug-server-host "127.0.0.1")

;; (setq chrome-ff--debug-server-port "6000")

(setq chrome-ff--get-target-template "{\"type\":\"getTarget\",\"to\":\"%s\"}")

(setq chrome-ff--attach-template "{\"type\":\"attach\",\"to\":\"%s\"}")

(setq chrome-ff--detach-template "{\"type\":\"detach\",\"to\":\"%s\"}")

(setq chrome-ff--navigate-to-template "{\"type\":\"navigateTo\",\"url\":\"%s\",\"to\":\"%s\"}")

(setq chrome-ff--get-tabs-payload "{\"to\":\"root\",\"type\":\"listTabs\"}")

(setq chrome-ff--get-process-payload "{\"type\":\"getProcess\",\"id\":0,\"to\":\"root\"}")

(setq chrome-ff--list-processes-payload "{\"type\":\"listProcesses\",\"to\":\"root\"}")

(setq chrome-ff--focus-template "{\"to\":\"%s\",\"type\":\"focus\"}")

(setq chrome-ff--eval-template "{\"to\":\"%s\",\"type\":\"evaluateJSAsync\",\"text\":\"%s\"}")

(setq chrome-ff--destroy-template "{\"to\":\"%s\",\"type\":\"destroy\"}")

(setq chrome-ff--proc-name "*chrome ff remote*")

(setq chrome-ff--last-point nil)

(setq chrome-ff--frame-length nil)

(setq chrome-ff--process nil)

(defun chrome-ff--format-packet (frame)
  (format "%s:%s" (length frame) frame))

(defun read-frame (proc)
  "Read frame from a remote firefox debugging session. Inspired by 
https://github.com/ruediger/emacs-firefox-remote/blob/master/firefox-remote.el"
  (with-current-buffer (process-buffer proc)
    (setq chrome-ff--frame-length 0)
    (accept-process-output proc 0 50)
    (if chrome-ff--last-point
        (goto-char chrome-ff--last-point)
      (goto-char (point-min)))
    (when (looking-at "[[:digit:]]+:")
      (setq chrome-ff-frame-length (+ (match-end 0)
                                      (string-to-number
                                       (buffer-substring (match-beginning 0)
                                                         (- (match-end 0) 1)))))
      (goto-char (match-end 0)))
    ;; TODO: Needs a timeout
    (while (< (point-max) (+ (match-end 0) chrome-ff--frame-length))
      (accept-process-output proc 0 50))

    (let ((obj (ignore-errors (json-read))))
      (when obj
        ;; (insert "\n")
        (setq chrome-ff--last-point (point)))
      obj)))


(defun chrome-ff--connect (port host)
  (with-current-buffer (get-buffer-create chrome-ff--proc-name) 
    (when chrome-ff--process
      (when (not (eq (process-status chrome-ff--process) 'closed))
        (delete-process chrome-ff--process)))
    (erase-buffer)
    (setq chrome-ff--process nil)
    (setq chrome-ff--last-point nil))

  (setq chrome-ff--process (open-network-stream (format "%s/%s" host port)
                                                chrome-ff--proc-name
                                                host
                                                port))
  ;; discard capabilities
  ;; (list chrome-ff--process (alist-get 'testConnectionPrefix
  ;;                                     (read-frame chrome-ff--process)))
  chrome-ff--process)

;; (defun chrome-ff--focus (proc-data tab)
;;   (let* ((proc (car proc-data))
;;          (tab-descriptor (format "%s%s" (cadr proc-data) (alist-get 'id tab))))
;;     (chrome-ff--list-tabs proc-data tab-descriptor)))


;; Eval javascript
;; >> {"type":"evaluateJSAsync","text":"alert(1+1)","eager":true,"to":"server1.conn21.consoleActor3723"}
(defun chrome-ff--eval (proc stmt)
  (let* ((resultid nil)
         (frame nil)
         (result nil)
         (proc-descriptor (chrome-ff--get-a-process-descriptor proc))
         (proc-target (chrome-ff--get-target proc
                                             proc-descriptor))
         (proc-actor (alist-get 'actor proc-target))
         (payload (format chrome-ff--eval-template
                          (alist-get 'consoleActor proc-target)
                          stmt)))

    (chrome-ff--attach proc proc-actor)
    (read-frame proc)
    (process-send-string proc (chrome-ff--format-packet payload))
    (while (eq result nil)
      (setq frame (read-frame proc))
      (setq result (if (equal (alist-get 'type frame) "evaluationResult")
                       (alist-get 'result frame)
                     nil)))
    (chrome-ff--detach proc proc-actor)
    result))
;; (chrome-ff--eval (chrome-ff--connect) "1+1")

(defun chrome-ff--get-root-process-actor (proc)
  ;; Get process
  ;; >> {"type":"getProcess","id":0,"to":"root"}
  ;; << {"processDescriptor":{"actor":"server1.conn21.processDescriptor31","id":0,"isParent":true},"from":"root"}
  (process-send-string proc
                       (chrome-ff--format-packet chrome-ff--get-process-payload))
  (alist-get 'actor (car (read-frame proc))))

(cl-defun chrome-ff--get-a-process-descriptor (proc &key (id 0))
  (cl-loop for proc-alist across (chrome-ff--list-processes proc)
           for proc-id = (alist-get 'id proc-alist)
           when (eq proc-id id)
           return (alist-get 'actor proc-alist) end))
;; (chrome-ff--get-a-process-actor (chrome-ff--connect))

(defun chrome-ff--get-target (proc descriptor &optional tag)
  (let ((payload (format chrome-ff--get-target-template
                         descriptor))
        (target nil))
    (process-send-string proc (chrome-ff--format-packet payload))
    (setq target (car (read-frame proc)))
    (if tag 
        (while (not (equal (car target) tag))
          (setq target (car (read-frame proc)))))
    target))

(defun chrome-ff--list-processes (proc)
  (process-send-string proc (chrome-ff--format-packet chrome-ff--list-processes-payload))
  (cdar (read-frame proc)))
;; (chrome-ff--list-processes (chrome-ff--connect))

(defun chrome-ff--attach (proc console-actor)
  (process-send-string proc
                       (chrome-ff--format-packet
                        (format chrome-ff--attach-template console-actor))))

(defun chrome-ff--detach (proc console-actor)
  (process-send-string proc
                       (chrome-ff--format-packet
                        (format chrome-ff--detach-template console-actor))))

;; (defun chrome-ff--get-tabs (proc)
;;   (let* ((preview (alist-get 'preview (chrome-ff--eval proc "gBrowser.tabs")))
;;          (items (alist-get 'items preview)))
;;     (cl-loop for tab-alist across items
;;              for label = (alist-get 'label tab-alist)
;;              for id = (alist-get '_tPos tab-alist))))
;; (chrome-ff--get-tabs (chrome-ff--connect))

(defun chrome-ff--list-tabs (proc)
  (process-send-string proc (chrome-ff--format-packet chrome-ff--get-tabs-payload))
  (let ((tabs nil))
    (setq tabs (alist-get 'tabs (read-frame proc)))
    (while (eq tabs nil)
      (setq tabs (alist-get 'tabs (read-frame proc))))
    tabs))
;; (chrome-ff--list-tabs (chrome-ff--connect))

(defun chrome-ff--get-tab-details (proc)
  (cl-loop with count = 0
           for tab across (chrome-ff--list-tabs proc)
           for actor = (alist-get 'actor tab)
           for tab-info = (chrome-ff--get-target proc actor 'frame)
           for tab-actor = (alist-get 'actor tab-info)
           when tab-info do (cl-incf count)
           when tab-info collect (list
                                  (cons 'id (alist-get 'browsingContextID
                                                       tab-info))
                                  (assoc 'actor tab-info)
                                  (assoc 'url tab-info)
                                  (assoc 'title tab-info))
           into tabs           
           finally return (cons count tabs)))
;; (chrome-ff--get-tab-details (chrome-ff--connect))

(defun chrome-ff--eval-for-tab (proc to-eval tab-id)
  (cl-loop with count = 0
           for tab in (cdr (chrome-ff--get-tab-details proc))
           for id = (alist-get 'id tab)
           when (equal tab-id id)
           do (chrome-ff--eval proc
                               (format to-eval count))
           end
           when tab do (cl-incf count)))

(defun chrome-ff--focus (proc tab-id)
  (chrome-ff--eval-for-tab proc "gBrowser.selectTabAtIndex(%d)" tab-id ))

(defun chrome-ff--delete-tab (proc tab-id)
  (chrome-ff--eval-for-tab proc "gBrowser.removeTab(gBrowser.tabs[%d])" tab-id))

(defun chrome-ff--view-source (proc tab-id)
  (cl-loop  for tab in (cdr (chrome-ff--get-tab-details proc))
            for id  = (alist-get 'id tab)
            when (equal tab-id id) do
            (process-send-string proc
                                 (chrome-ff--format-packet
                                  (format chrome-ff--navigate-to-template
                                          (format "view-source:%s" (alist-get 'url tab))
                                          (alist-get 'actor tab))))
            end))


(defun chrome--get-tabs (port host)
  (chrome-ff--get-tab-details (chrome-ff--connect port host)))


(defun chrome--delete (tab)
  ;; don't try to delete things that are already in purgatory
  (unless (chrome-tab-is-deleted tab)
    (chrome-ff--delete-tab (chrome-ff--connect (chrome-tab-port tab)
                                               (chrome-tab-host tab))
                           (chrome-tab-id tab))
    (setf (chrome-tab-is-deleted tab) t)))

(defun chrome--visit (tab)
  (setq facuman/test tab)
  (chrome-ff--focus (chrome-ff--connect (chrome-tab-port tab) 
                                        (chrome-tab-host tab))
                    (chrome-tab-id tab)))

(defun chrome--view-source (tab)
  (chrome-ff--view-source (chrome-ff--connect (chrome-tab-port tab)
                                              (chrome-tab-host tab))
                          (format "view-source:%s" (chrome-tab-url tab))))
;; (setq chrome-sessions '())
