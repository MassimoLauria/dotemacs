<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"><head><title>EmacsWiki: lorem-ipsum.el</title><link rel="alternate" type="application/wiki" title="Edit this page" href="http://www.emacswiki.org/emacs?action=edit;id=lorem-ipsum.el" /><link type="text/css" rel="stylesheet" href="/emacs/wiki.css" /><meta name="robots" content="INDEX,FOLLOW" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki" href="http://www.emacswiki.org/emacs?action=rss" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki: lorem-ipsum.el" href="http://www.emacswiki.org/emacs?action=rss;rcidonly=lorem-ipsum.el" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content"
      href="http://www.emacswiki.org/emacs/full.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content and diff"
      href="http://www.emacswiki.org/emacs/full-diff.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki including minor differences"
      href="http://www.emacswiki.org/emacs/minor-edits.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Changes for lorem-ipsum.el only"
      href="http://www.emacswiki.org/emacs?action=rss;rcidonly=lorem-ipsum.el" /><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/></head><body class="http://www.emacswiki.org/emacs"><div class="header"><a class="logo" href="http://www.emacswiki.org/emacs/SiteMap"><img class="logo" src="/emacs_logo.png" alt="[Home]" /></a><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span>
<!-- Google CSE Search Box Begins  -->
<form class="tiny" action="http://www.google.com/cse" id="searchbox_004774160799092323420:6-ff2s0o6yi"><p>
<input type="hidden" name="cx" value="004774160799092323420:6-ff2s0o6yi" />
<input type="text" name="q" size="25" />
<input type="submit" name="sa" value="Search" />
</p></form>
<script type="text/javascript" src="http://www.google.com/coop/cse/brand?form=searchbox_004774160799092323420%3A6-ff2s0o6yi"></script>
<!-- Google CSE Search Box Ends -->
<br /><span class="specialdays">Sierra Leone, Republic Day</span><h1><a title="Click to search for references to this page" rel="nofollow" href="http://www.google.com/cse?cx=004774160799092323420:6-ff2s0o6yi&amp;q=%22lorem-ipsum.el%22">lorem-ipsum.el</a></h1></div><div class="wrapper"><div class="content browse"><p class="download"><a href="download/lorem-ipsum.el">Download</a></p><pre class="code"><span class="linecomment">;;; lorem-ipsum.el --- Insert dummy pseudo Latin text.</span>
<span class="linecomment">;; Author & Maintainer: Jean-Philippe Theberge (jphil21@sourceforge.net)</span>
<span class="linecomment">;; Special Thanks: The emacswiki users, the #emacs@freenode.net citizens </span>
<span class="linecomment">;;                 and Marcus Tullius Cicero</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; version :</span>
(defconst lorem-ipsum-version "<span class="quote">0.1</span>")
<span class="linecomment">;; last update: 16/09/2003</span>

<span class="linecomment">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</span>
<span class="linecomment">;;; Copyright (c) 2003 Jean-Philippe Theberge</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; This file is not (yet?) part of GNU Emacs.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; This file is free software; you can redistribute it and/or modify</span>
<span class="linecomment">;; it under the terms of the GNU General Public License as published by</span>
<span class="linecomment">;; the Free Software Foundation; either version 2, or (at your option)</span>
<span class="linecomment">;; any later version.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; GNU Emacs is distributed in the hope that it will be useful,</span>
<span class="linecomment">;; but WITHOUT ANY WARRANTY; without even the implied warranty of</span>
<span class="linecomment">;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the</span>
<span class="linecomment">;; GNU General Public License for more details.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; You should have received a copy of the GNU General Public License</span>
<span class="linecomment">;; along with GNU Emacs; see the file COPYING.  If not, write to the</span>
<span class="linecomment">;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,</span>
<span class="linecomment">;; Boston, MA 02111-1307, USA.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</span>

(defconst Lorem-ipsum-text
  '(("<span class="quote">Lorem ipsum dolor sit amet, consectetuer adipiscing elit.</span>"
     "<span class="quote">Donec hendrerit tempor tellus.</span>"
     "<span class="quote">Donec pretium posuere tellus.</span>"
     "<span class="quote">Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus.</span>"
     "<span class="quote">Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.</span>"
     "<span class="quote">Nulla posuere.</span>"
     "<span class="quote">Donec vitae dolor.</span>"
     "<span class="quote">Nullam tristique diam non turpis.</span>"
     "<span class="quote">Cras placerat accumsan nulla.</span>"
     "<span class="quote">Nullam rutrum.</span>"
     "<span class="quote">Nam vestibulum accumsan nisl.</span>")

    ("<span class="quote">Pellentesque dapibus suscipit ligula.</span>"
     "<span class="quote">Donec posuere augue in quam.</span>"
     "<span class="quote">Etiam vel tortor sodales tellus ultricies commodo.</span>"
     "<span class="quote">Suspendisse potenti.</span>"
     "<span class="quote">Aenean in sem ac leo mollis blandit.</span>"
     "<span class="quote">Donec neque quam, dignissim in, mollis nec, sagittis eu, wisi.</span>"
     "<span class="quote">Phasellus lacus.</span>"
     "<span class="quote">Etiam laoreet quam sed arcu.</span>"
     "<span class="quote">Phasellus at dui in ligula mollis ultricies.</span>"
     "<span class="quote">Integer placerat tristique nisl.</span>"
     "<span class="quote">Praesent augue.</span>"
     "<span class="quote">Fusce commodo.</span>"
     "<span class="quote">Vestibulum convallis, lorem a tempus semper, dui dui euismod elit, vitae placerat urna tortor vitae lacus.</span>"
     "<span class="quote">Nullam libero mauris, consequat quis, varius et, dictum id, arcu.</span>"
     "<span class="quote">Mauris mollis tincidunt felis.</span>"
     "<span class="quote">Aliquam feugiat tellus ut neque.</span>"
     "<span class="quote">Nulla facilisis, risus a rhoncus fermentum, tellus tellus lacinia purus, et dictum nunc justo sit amet elit.</span>")

    ("<span class="quote">Aliquam erat volutpat.</span>"
     "<span class="quote">Nunc eleifend leo vitae magna.</span>"
     "<span class="quote">In id erat non orci commodo lobortis.</span>"
     "<span class="quote">Proin neque massa, cursus ut, gravida ut, lobortis eget, lacus.</span>"
     "<span class="quote">Sed diam.</span>"
     "<span class="quote">Praesent fermentum tempor tellus.</span>"
     "<span class="quote">Nullam tempus.</span>"
     "<span class="quote">Mauris ac felis vel velit tristique imperdiet.</span>"
     "<span class="quote">Donec at pede.</span>"
     "<span class="quote">Etiam vel neque nec dui dignissim bibendum.</span>"
     "<span class="quote">Vivamus id enim.</span>"
     "<span class="quote">Phasellus neque orci, porta a, aliquet quis, semper a, massa.</span>"
     "<span class="quote">Phasellus purus.</span>"
     "<span class="quote">Pellentesque tristique imperdiet tortor.</span>"
     "<span class="quote">Nam euismod tellus id erat.</span>")

    ("<span class="quote">Nullam eu ante vel est convallis dignissim.</span>"
     "<span class="quote">Fusce suscipit, wisi nec facilisis facilisis, est dui fermentum leo, quis tempor ligula erat quis odio.</span>"
     "<span class="quote">Nunc porta vulputate tellus.</span>"
     "<span class="quote">Nunc rutrum turpis sed pede.</span>"
     "<span class="quote">Sed bibendum.</span>"
     "<span class="quote">Aliquam posuere.</span>"
     "<span class="quote">Nunc aliquet, augue nec adipiscing interdum, lacus tellus malesuada massa, quis varius mi purus non odio.</span>"
     "<span class="quote">Pellentesque condimentum, magna ut suscipit hendrerit, ipsum augue ornare nulla, non luctus diam neque sit amet urna.</span>"
     "<span class="quote">Curabitur vulputate vestibulum lorem.</span>"
     "<span class="quote">Fusce sagittis, libero non molestie mollis, magna orci ultrices dolor, at vulputate neque nulla lacinia eros.</span>"
     "<span class="quote">Sed id ligula quis est convallis tempor.</span>"
     "<span class="quote">Curabitur lacinia pulvinar nibh.</span>"
     "<span class="quote">Nam a sapien.</span>")))

(defvar Lorem-ipsum-paragraph-separator "<span class="quote">\n\n</span>")
(defvar Lorem-ipsum-sentence-separator "<span class="quote">  </span>")
(defvar Lorem-ipsum-list-beginning "<span class="quote"></span>")
(defvar Lorem-ipsum-list-bullet "<span class="quote">* </span>")
(defvar Lorem-ipsum-list-item-end "<span class="quote">\n</span>")
(defvar Lorem-ipsum-list-end "<span class="quote"></span>")

(make-variable-buffer-local 'Lorem-ipsum-paragraph-separator)
(make-variable-buffer-local 'Lorem-ipsum-sentence-separator)
(make-variable-buffer-local 'Lorem-ipsum-list-beginning)
(make-variable-buffer-local 'Lorem-ipsum-list-bullet)
(make-variable-buffer-local 'Lorem-ipsum-list-item-end)
(make-variable-buffer-local 'Lorem-ipsum-list-end)

(add-hook 'sgml-mode-hook (lambda ()
			    (setq Lorem-ipsum-paragraph-separator "<span class="quote">&lt;br&gt;&lt;br&gt;\n</span>"
				  Lorem-ipsum-sentence-separator "<span class="quote">&nbsp;&nbsp;</span>"
				  Lorem-ipsum-list-beginning "<span class="quote">&lt;ul&gt;\n</span>"
				  Lorem-ipsum-list-bullet "<span class="quote">&lt;li&gt;</span>"
				  Lorem-ipsum-list-item-end "<span class="quote">&lt;/li&gt;\n</span>"
				  Lorem-ipsum-list-end "<span class="quote">&lt;/ul&gt;\n</span>")))

(defun Lorem-ipsum-insert-paragraphs (&optional num)
  (interactive "<span class="quote">p</span>")
  (if (not num)(setq num 1))
  (if (&gt; num 0)
      (progn
	(insert (concat 
		 (mapconcat 'identity 
			    (nth (if (interactive-p) 0 (random (length Lorem-ipsum-text)))
				 Lorem-ipsum-text) "<span class="quote"> </span>")
		 Lorem-ipsum-paragraph-separator))
	(Lorem-ipsum-insert-paragraphs (- num 1)))))

(defun Lorem-ipsum-insert-sentences (&optional num)
  (interactive "<span class="quote">p</span>")
  (if (not num)(setq num 1))
  (if (&gt; num 0)
      (progn
	(let ((para 
	       (nth (if (interactive-p) 0 (random (length Lorem-ipsum-text))) Lorem-ipsum-text)))
	  (insert (concat (nth (if (interactive-p) 0 (random (length para))) para) Lorem-ipsum-sentence-separator)))
	(Lorem-ipsum-insert-sentences (- num 1)))))
	  
(defun Lorem-ipsum-insert-list (&optional num)
  (interactive "<span class="quote">p</span>")
  (if (not num)(setq num 1))
  (if (&gt; num 0)
      (progn
	(if (interactive-p) (insert Lorem-ipsum-list-beginning))
	(let ((para (nth (if (interactive-p) 0 (random (length Lorem-ipsum-text))) Lorem-ipsum-text)))
	  (insert (concat Lorem-ipsum-list-bullet 
			  (nth (if (interactive-p) 0 (random (length para))) para) 
			  Lorem-ipsum-list-item-end)))
	(Lorem-ipsum-insert-list (- num 1)))
    (insert Lorem-ipsum-list-end)))


(provide 'lorem-ipsum)

<span class="linecomment">;;; lorem-ipsum.el ends here</span></span></pre></div><div class="wrapper close"></div></div><div class="footer"><hr /><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span><span class="translation bar"><br />  <a class="translation new" rel="nofollow" href="http://www.emacswiki.org/emacs?action=translate;id=lorem-ipsum.el;missing=de_en_es_fr_it_ja_ko_pt_ru_se_zh">Add Translation</a></span><span class="edit bar"><br /> <a class="edit" accesskey="e" title="Click to edit this page" rel="nofollow" href="http://www.emacswiki.org/emacs?action=edit;id=lorem-ipsum.el">Edit this page</a> <a class="history" rel="nofollow" href="http://www.emacswiki.org/emacs?action=history;id=lorem-ipsum.el">View other revisions</a> <a class="admin" rel="nofollow" href="http://www.emacswiki.org/emacs?action=admin;id=lorem-ipsum.el">Administration</a></span><span class="time"><br /> Last edited 2005-10-13 17:56 UTC by <a class="author" title="from 217-162-112-104.dclient.hispeed.ch" href="http://www.emacswiki.org/emacs/AlexSchroeder">AlexSchroeder</a> <a class="diff" rel="nofollow" href="http://www.emacswiki.org/emacs?action=browse;diff=2;id=lorem-ipsum.el">(diff)</a></span><div style="float:right; margin-left:1ex;">
<!-- Creative Commons License -->
<a href="http://creativecommons.org/licenses/GPL/2.0/"><img alt="CC-GNU GPL" style="border:none" src="/pics/cc-GPL-a.png" /></a>
<!-- /Creative Commons License -->
</div>

<!--
<rdf:RDF xmlns="http://web.resource.org/cc/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
<Work rdf:about="">
   <license rdf:resource="http://creativecommons.org/licenses/GPL/2.0/" />
  <dc:type rdf:resource="http://purl.org/dc/dcmitype/Software" />
</Work>

<License rdf:about="http://creativecommons.org/licenses/GPL/2.0/">
   <permits rdf:resource="http://web.resource.org/cc/Reproduction" />
   <permits rdf:resource="http://web.resource.org/cc/Distribution" />
   <requires rdf:resource="http://web.resource.org/cc/Notice" />
   <permits rdf:resource="http://web.resource.org/cc/DerivativeWorks" />
   <requires rdf:resource="http://web.resource.org/cc/ShareAlike" />
   <requires rdf:resource="http://web.resource.org/cc/SourceCode" />
</License>
</rdf:RDF>
-->

<p class="legal">
This work is licensed to you under version 2 of the
<a href="http://www.gnu.org/">GNU</a> <a href="/GPL">General Public License</a>.
Alternatively, you may choose to receive this work under any other
license that grants the right to use, copy, modify, and/or distribute
the work, as long as that license imposes the restriction that
derivative works have to grant the same rights and impose the same
restriction. For example, you may choose to receive this work under
the
<a href="http://www.gnu.org/">GNU</a>
<a href="/FDL">Free Documentation License</a>, the
<a href="http://creativecommons.org/">CreativeCommons</a>
<a href="http://creativecommons.org/licenses/sa/1.0/">ShareAlike</a>
License, the XEmacs manual license, or
<a href="/OLD">similar licenses</a>.
</p>
</div>
</body>
</html>
