$set nover
$copy/log sys$input NEWS.ALT
$deck

From: ccs@aber.ac.uk (Christopher Samuel)
Date:  2-OCT-1992  11:36:37
Description: Creating a new "alt" group -- guidelines

Archive-name: alt-config-guide
Version: 1.2
Last-modified: Wed Sep  2 16:31:55 GMT 1992
 
 
 
		Guidelines for the creation of an "alt" group.
 
There are no rules or guidelines for creation of "alt" groups, However
there does appear to be an established procedure which follows.  First a
quick bit of common-sense on choosing the name:
 
When choosing a name for a group please note the only commandment: Thou
shalt not choose a group name which may cause network harm or harm to a
local machine. 
 
          Examples:
 
          alt.fan.enya.puke.puke.pukeSender:
 
          [preceding line to Sender had <CR> deleted; also the
           trailing : can cause problems in some news systems]
 
          alt.verylonggroupnamethathasadirectorylongerthanmost\
          machinessupportsotherehaha.very.funny
 
          alt.[insert300charactershere].very.long.group.name.\
          that.is.too.big.for.newsrc
 
          alt.*.this.name.has.bad.characters.in.it
 
          alt..double.dot.group.name
 
 
		Now the Guidelines:
		-------------------
 
       1) Propose a new alt group in alt.config.  The proposal
          should include a charter or purpose for the new group, and
          some demonstration of the need for the group.  It is best to
          make it clear in your subject line that you are proposing a
          new group. Be prepared to explain why an existing group cannot
          be used for this purpose, and why the group should be in "alt"
          rather than in one of the mainstream hierarchies (like
          "rec", "sci", etc.).  Avoiding the complexity of the
          mainstream group creation procedure is not a very good
          reason, groups should not be created in "alt" just because
          it's easier.  Don't forget that mainstream groups can also
          be created by the "trial" mechanism.  Many sites do not get
          any alt groups, so if you are proposing a serious group, it
          is worth the effort to try to get it into a mainstream
          hierarchy.
 
       2) See what the alt.net.opinion of the new group is.  Wait a
          few days for replies to trickle in from the far corners of
          the net.  If the consensus (however you determine that) is
          that the group should be created, then proceed to step 3.
 
          (these first two steps are often ignored, which usually
          leads to unpleasantness in step 4 below)
 
       3) Post a "newgroup" control message.  If you don't know
          how to do this, check with your news administrator.  If you
          ARE your news administrator, and you can't figure it out
          from the documentation you have (or don't have any
          documentation) send me mail and I will help you.  NOTE that
          many sites do NOT automatically honor "newgroup" and
          "rmgroup" control messages, the news software at these sites
          will send mail to the news administrator, who will evaluate
          your request and decide whether or not to create the group.
          It may take a couple of days for the control message to
          propagate and be acted upon, so don't expect instant
          availability of the new group, particularly if you post the
          control message on a Friday night.
 
          NB:	It is good manners to put a description of the new
          	newsgroup into the newgroup message, along with a
          	one-line description suitable for inclusion into the
          	newsgroups file.
 
       4) Let the individual site news administrators decide
          whether to honor your "newgroup" message.  Most admins
          prefer that the message come from a verifiable account,
          messages which are obviously forged, or have not been
          discussed in alt.config and contain no explanation will
          probably not be honored by many sites.  Persons opposed to
          the group, or admins who feel that the newgroup message was
          a forgery may send out "rmgroup" messages to try to sabotage
          the group.  It may take several iterations of this process
          to firmly establish the new group.  It has been humorously
          suggested that only alt groups which get 100 more "newgroup"
          than "rmgroup" messages should be established.  However,
          these "rmgroup wars" are annoying to news administrators,
          and reduce the overall acceptance (and distribution) of the
          "alt" hierarchy.  This is the reason that steps 1 and 2
          above are important.
 
 
This may sound like a lot of rigamarole, and it is.  The purpose is to
discourage creation of alt groups that might be better off as mainstream
groups, or that might be better of left uncreated.
 
Don't take this all too seriously, though.  The "alt" net is the last
remaining refuge away from the control freaks, namespace purists and
net.cops (like myself) that maintain and enforce the mainstream
newsgroup guidelines.
 
There is still some room for spontaneity out here on the "alt" frontier. 
Successful groups have been created without following these suggestions. 
Almost any non-forged, serious newgroup message will at least be
considered by most news admins.  Some groups have been created just on a
whim.  The concept behind the group better be good (or a least
entertaining), though!
 
[ If you want more information on mainstream group creation see the post
  "How to Create a New Newsgroup" posted to news.answers, news.admin and
  news.groups. ]
 
-- 
 Christopher Samuel, c/o Computer Unit, UCW Aberystwyth, Aberystwyth, WALES
  RFC: ccs@aber.ac.uk   UUCP: *!mcsun!uknet!aber!ccs   JNT: ccs@uk.ac.aber
          Deddf Iaith Newydd i Gymru | New Language Act for Wales

From: ccs@aber.ac.uk (Christopher Samuel)
Date:  2-OCT-1992  11:36:37
Description: Creating a new "alt" group -- guidelines

Archive-name: alt-config-guide
Version: 1.2
Last-modified: Wed Sep  2 16:31:55 GMT 1992
 
 
 
		Guidelines for the creation of an "alt" group.
 
There are no rules or guidelines for creation of "alt" groups, However
there does appear to be an established procedure which follows.  First a
quick bit of common-sense on choosing the name:
 
When choosing a name for a group please note the only commandment: Thou
shalt not choose a group name which may cause network harm or harm to a
local machine. 
 
          Examples:
 
          alt.fan.enya.puke.puke.pukeSender:
 
          [preceding line to Sender had <CR> deleted; also the
           trailing : can cause problems in some news systems]
 
          alt.verylonggroupnamethathasadirectorylongerthanmost\
          machinessupportsotherehaha.very.funny
 
          alt.[insert300charactershere].very.long.group.name.\
          that.is.too.big.for.newsrc
 
          alt.*.this.name.has.bad.characters.in.it
 
          alt..double.dot.group.name
 
 
		Now the Guidelines:
		-------------------
 
       1) Propose a new alt group in alt.config.  The proposal
          should include a charter or purpose for the new group, and
          some demonstration of the need for the group.  It is best to
          make it clear in your subject line that you are proposing a
          new group. Be prepared to explain why an existing group cannot
          be used for this purpose, and why the group should be in "alt"
          rather than in one of the mainstream hierarchies (like
          "rec", "sci", etc.).  Avoiding the complexity of the
          mainstream group creation procedure is not a very good
          reason, groups should not be created in "alt" just because
          it's easier.  Don't forget that mainstream groups can also
          be created by the "trial" mechanism.  Many sites do not get
          any alt groups, so if you are proposing a serious group, it
          is worth the effort to try to get it into a mainstream
          hierarchy.
 
       2) See what the alt.net.opinion of the new group is.  Wait a
          few days for replies to trickle in from the far corners of
          the net.  If the consensus (however you determine that) is
          that the group should be created, then proceed to step 3.
 
          (these first two steps are often ignored, which usually
          leads to unpleasantness in step 4 below)
 
       3) Post a "newgroup" control message.  If you don't know
          how to do this, check with your news administrator.  If you
          ARE your news administrator, and you can't figure it out
          from the documentation you have (or don't have any
          documentation) send me mail and I will help you.  NOTE that
          many sites do NOT automatically honor "newgroup" and
          "rmgroup" control messages, the news software at these sites
          will send mail to the news administrator, who will evaluate
          your request and decide whether or not to create the group.
          It may take a couple of days for the control message to
          propagate and be acted upon, so don't expect instant
          availability of the new group, particularly if you post the
          control message on a Friday night.
 
          NB:	It is good manners to put a description of the new
          	newsgroup into the newgroup message, along with a
          	one-line description suitable for inclusion into the
          	newsgroups file.
 
       4) Let the individual site news administrators decide
          whether to honor your "newgroup" message.  Most admins
          prefer that the message come from a verifiable account,
          messages which are obviously forged, or have not been
          discussed in alt.config and contain no explanation will
          probably not be honored by many sites.  Persons opposed to
          the group, or admins who feel that the newgroup message was
          a forgery may send out "rmgroup" messages to try to sabotage
          the group.  It may take several iterations of this process
          to firmly establish the new group.  It has been humorously
          suggested that only alt groups which get 100 more "newgroup"
          than "rmgroup" messages should be established.  However,
          these "rmgroup wars" are annoying to news administrators,
          and reduce the overall acceptance (and distribution) of the
          "alt" hierarchy.  This is the reason that steps 1 and 2
          above are important.
 
 
This may sound like a lot of rigamarole, and it is.  The purpose is to
discourage creation of alt groups that might be better off as mainstream
groups, or that might be better of left uncreated.
 
Don't take this all too seriously, though.  The "alt" net is the last
remaining refuge away from the control freaks, namespace purists and
net.cops (like myself) that maintain and enforce the mainstream
newsgroup guidelines.
 
There is still some room for spontaneity out here on the "alt" frontier. 
Successful groups have been created without following these suggestions. 
Almost any non-forged, serious newgroup message will at least be
considered by most news admins.  Some groups have been created just on a
whim.  The concept behind the group better be good (or a least
entertaining), though!
 
[ If you want more information on mainstream group creation see the post
  "How to Create a New Newsgroup" posted to news.answers, news.admin and
  news.groups. ]
 
-- 
 Christopher Samuel, c/o Computer Unit, UCW Aberystwyth, Aberystwyth, WALES
  RFC: ccs@aber.ac.uk   UUCP: *!mcsun!uknet!aber!ccs   JNT: ccs@uk.ac.aber
          Deddf Iaith Newydd i Gymru | New Language Act for Wales
$eod 
$copy/log sys$input NEWS.CREATE
$deck
From: tale@uunet.uu.net (David C Lawrence)
Date: 19-OCT-1992  00:15:29
Description: How to Create a New Usenet Newsgroup

Archive-name: creating-newsgroups/part1
Original-author: woods@ncar.ucar.edu (Greg Woods)
Last-change: 23 Sep 1992 by spaf@cs.purdue.edu (Gene Spafford)
 
				 GUIDELINES FOR USENET GROUP CREATION
 
REQUIREMENTS FOR GROUP CREATION:
 
   These are guidelines that have been generally agreed upon across
USENET as appropriate for following in the creating of new newsgroups in
the "standard" USENET newsgroup hierarchy. They are NOT intended as 
guidelines for setting USENET policy other than group creations, and they
are not intended to apply to "alternate" or local news hierarchies. The 
part of the namespace affected is comp, news, sci, misc, soc, talk, rec,
which are the most widely-distributed areas of the USENET hierarchy.
   Any group creation request which follows these guidelines to a
successful result should be honored, and any request which fails to
follow these procedures or to obtain a successful result from doing so
should be dropped, except under extraordinary circumstances.  The
reason these are called guidelines and not absolute rules is that it is
not possible to predict in advance what "extraordinary circumstances"
are or how they might arise.
   It should be pointed out here that, as always, the decision whether or not
to create a newsgroup on a given machine rests with the administrator of that
machine. These guidelines are intended merely as an aid in making those
decisions.
 
 
The Discussion
 
1) A request for discussion on creation of a new newsgroup should be posted to
   news.announce.newgroups, and also to any other groups or mailing lists at
   all related to the proposed topic if desired.  The group is moderated, and
   the Followup-to: header will be set so that the actual discussion takes
   place only in news.groups.  Users on sites which have difficulty posting to
   moderated groups may mail submissions intended for news.announce.newgroups
   to announce-newgroups@uunet.uu.net.
 
   The article should be cross-posted among the newsgroups, including
   news.announce.newgroups, rather than posted as separate articles.  Note that
   standard behaviour for posting software is to not present the articles in
   any groups when cross-posted to a moderated group; the moderator will handle
   that for you.
 
2) The name and charter of the proposed group and whether it will be moderated
   or unmoderated (and if the former, who the moderator(s) will be) should be
   determined during the discussion period. If there is no general agreement on
   these points among the proponents of a new group at the end of 30 days of
   discussion, the discussion should be taken offline (into mail instead of
   news.groups) and the proponents should iron out the details among
   themselves.  Once that is done, a new, more specific proposal may be made,
   going back to step 1) above.  
 
3) Group advocates seeking help in choosing a name to suit the proposed
   charter, or looking for any other guidance in the creation procedure, can
   send a message to group-advice@uunet.uu.net; a few seasoned news administrators
   are available through this address.
 
The Vote
 
1) AFTER the discussion period, if it has been determined that a new group is
   really desired, a name and charter are agreed upon, and it has been
   determined whether the group will be moderated and if so who will
   moderate it, a call for votes may be posted to news.announce.newgroups and
   any other groups or mailing lists that the original request for discussion
   might have been posted to. There should be minimal delay between the
   end of the discussion period and the issuing of a call for votes.
   The call for votes should include clear instructions for how to cast
   a vote. It must be as clearly explained and as easy to do to cast a
   vote for creation as against it, and vice versa.  It is explicitly
   permitted to set up two separate addresses to mail yes and no votes
   to provided that they are on the same machine, to set up an address
   different than that the article was posted from to mail votes to, or
   to just accept replies to the call for votes article, as long as it
   is clearly and explicitly stated in the call for votes article how
   to cast a vote.  If two addresses are used for a vote, the reply
   address must process and accept both yes and no votes OR reject
   them both.
 
2) The voting period should last for at least 21 days and no more than 31
   days, no matter what the preliminary results of the vote are. The exact
   date that the voting period will end should be stated in the call for
   votes. Only votes that arrive on the vote-taker's machine prior to this
   date will be counted.
 
3) A couple of repeats of the call for votes may be posted during the vote, 
   provided that they contain similar clear, unbiased instructions for
   casting a vote as the original, and provided that it is really a repeat
   of the call for votes on the SAME proposal (see #5 below). Partial vote
   results should NOT be included; only a statement of the specific new
   group proposal, that a vote is in progress on it, and how to cast a vote.
   It is permitted to post a "mass acknowledgement" in which all the names
   of those from whom votes have been received are posted, as long as no
   indication is made of which way anybody voted until the voting period
   is officially over.
 
4) ONLY votes MAILED to the vote-taker will count. Votes posted to the net
   for any reason (including inability to get mail to the vote-taker) and 
   proxy votes (such as having a mailing list maintainer claim a vote for 
   each member of the list) will not be counted.
 
5) Votes may not be transferred to other, similar proposals. A vote shall
   count only for the EXACT proposal that it is a response to. In particular,
   a vote for or against a newsgroup under one name shall NOT be counted as
   a vote for or against a newsgroup with a different name or charter,
   a different moderated/unmoderated status or (if moderated) a different
   moderator or set of moderators.
 
6) Votes MUST be explicit; they should be of the form "I vote for the
   group foo.bar as proposed" or "I vote against the group foo.bar
   as proposed". The wording doesn't have to be exact, it just needs to
   be unambiguous. In particular, statements of the form "I would vote
   for this group if..." should be considered comments only and not
   counted as votes.
 
7) A vote should be run only for a single group proposal.  Attempts to create
   multiple groups should be handled by running multiple parallel votes rather
   than one vote to create all of the groups.
 
The Result
 
1) At the completion of the voting period, the vote taker must post the
   vote tally and the E-mail addresses and (if available) names of the voters
   received to news.announce.newgroups and any other groups or mailing lists
   to which the original call for votes was posted. The tally should include
   a statement of which way each voter voted so that the results can be
   verified.
 
2) AFTER the vote result is posted, there will be a 5 day waiting period,
   beginning when the voting results actually appear in 
   news.announce.newgroups, during which the net will have a chance to
   correct any errors in the voter list or the voting procedure.
 
3) AFTER the waiting period, and if there were no serious objections that might
   invalidate the vote, and if 100 more valid YES/create votes are received
   than NO/don't create AND at least 2/3 of the total number of valid votes
   received are in favor of creation, a newgroup control message may be sent 
   out.  If the 100 vote margin or 2/3 percentage is not met, the group should 
   not be created.
 
4) The newgroup message will be sent by the news.announce.newgroups moderator
   at the end of the waiting period of a successful vote.  If the new group is
   moderated, the vote-taker should send a message during the waiting period to
   Gene Spafford <spaf@cs.purdue.edu> and David C. Lawrence <tale@uunet.uu.net> with
   both the moderator's contact address and the group's submission address.
 
5) A proposal which has failed under point (3) above should not again be
   brought up for discussion until at least six months have passed from the
   close of the vote.  This limitation does not apply to proposals which never
   went to vote.
 
$eod 
$copy/log sys$input NEWS.MODERATORS
$deck
comp.ai.nlang-know-rep		nl-kr@cs.rpi.edu
comp.ai.vision			vision-list@ads.com
comp.archives			comp-archives@msen.com
comp.binaries.acorn		cba@acorn.co.nz
comp.binaries.amiga		amiga@uunet.uu.net
comp.binaries.atari.st		atari-binaries@hyperion.com
comp.binaries.ibm.pc		cbip@cs.ulowell.edu
comp.binaries.mac		macintosh%felix.uucp@uunet.uu.net
comp.binaries.os2		os2bin@csd4.csd.uwm.edu
comp.bugs.4bsd.ucb-fixes	ucb-fixes@okeeffe.berkeley.edu
comp.compilers			compilers@iecc.cambridge.ma.us
comp.dcom.telecom		telecom@eecs.nwu.edu
comp.doc			comp-doc@ucsd.edu
comp.doc.techreports		compdoc-techreports@ftp.cse.ucsc.edu
comp.graphics.research		graphics@scri1.scri.fsu.edu
comp.internet.library		library@axon.cwru.edu
comp.lang.sigplan		sigplan@bellcore.com
comp.laser-printers		laser-lovers@brillig.umd.edu
comp.mail.maps			uucpmap@rutgers.edu
comp.newprod			newprod@chg.mcd.mot.com
comp.org.eff.news		effnews@eff.org
comp.org.fidonet		pozar@hop.toad.com
comp.os.ms-windows.announce	infidel+win-announce@pitt.edu
comp.os.research		osr@ftp.cse.ucsc.edu
comp.parallel			hypercube@hubcap.clemson.edu
comp.patents			patents@cs.su.oz.au
comp.protocols.kermit		info-kermit@watsun.cc.columbia.edu
comp.research.japan		japan@cs.arizona.edu
comp.risks			risks@csl.sri.com
comp.simulation			simulation@uflorida.cis.ufl.edu
comp.society			socicom@auvm.american.edu
comp.society.cu-digest		tk0jut2@mvs.cso.niu.edu
comp.society.folklore		folklore@snark.thyrsus.com
comp.society.privacy		comp-privacy@pica.army.mil
comp.sources.3b1		comp-sources-3b1@galaxia.network23.com
comp.sources.acorn		cba@acorn.co.nz
comp.sources.amiga		amiga@uunet.uu.net
comp.sources.apple2		jac@paul.rutgers.edu
comp.sources.atari.st		atari-sources@hyperion.com
comp.sources.games		games@saab.cna.tek.com
comp.sources.hp48		hp48@seq.uncwil.edu
comp.sources.mac		macintosh%felix.uucp@uunet.uu.net
comp.sources.misc		sources-misc@uunet.uu.net
comp.sources.reviewed		csr@calvin.dgbt.doc.ca
comp.sources.sun		sun-sources@topaz.rutgers.edu
comp.sources.unix		unix-sources-moderator@pa.dec.com
comp.sources.x			x-sources@msi.com
comp.std.announce		klensin@infoods.mit.edu
comp.std.mumps			std-mumps@pfcs.com
comp.std.unix			std-unix@uunet.uu.net
comp.sys.acorn.announce		announce@acorn.co.uk
comp.sys.amiga.announce		announce@cs.ucdavis.edu
comp.sys.amiga.reviews		amiga-reviews-submissions@math.uh.edu
comp.sys.concurrent		concurrent@bdcsys.suvl.ca.us
comp.sys.ibm.pc.digest		info-ibmpc@simtel20.army.mil
comp.sys.m68k.pc		info-68k@ucbvax.berkeley.edu
comp.sys.mac.announce		csma@rascal.ics.utexas.edu
comp.sys.mac.digest		info-mac@sumex-aim.stanford.edu
comp.sys.next.announce		csn-announce@liveware.com
comp.sys.sun.announce		sun-announce@sunworld.com
comp.theory.info-retrieval	ir-l%uccvma.bitnet@berkeley.edu
comp.virus			krvw@cert.org
comp.windows.x.announce		xannounce@expo.lcs.mit.edu
misc.activism.progressive	map@pencil.cs.missouri.edu
misc.handicap			handicap@bunker.shel.isc-br.com
misc.news.southasia		surekha@nyx.cs.du.edu
news.admin.technical		natech@zorch.sf-bay.org
news.announce.conferences	nac@tekbspa.tss.com
news.announce.important		announce@stargate.com
news.announce.newgroups		announce-newgroups@rpi.edu
news.announce.newusers		spaf@cs.purdue.edu
news.answers			news-answers@mit.edu
news.lists			news-lists-request@cs.purdue.edu
news.lists.ps-maps		reid@decwrl.dec.com
rec.arts.cinema			cinema@zerkalo.harvard.edu
rec.arts.comics.info		info_comic@dartmouth.edu
rec.arts.erotica		erotica@telly.on.ca
rec.arts.movies.reviews		movies@mtgzy.att.com
rec.arts.sf.announce		sf-announce@zorch.sf-bay.org
rec.arts.sf.reviews		sf-reviews@presto.ig.com
rec.arts.startrek.info		trek-info@dweeb.fx.com
rec.audio.high-end		info-high-audio@csd4.csd.uwm.edu
rec.food.recipes		recipes@mthvax.cs.miami.edu
rec.games.cyber			cyberrpg@veritas.com
rec.games.frp.announce		rg-frp-announce@magnus.acs.ohio-state.edu
rec.games.frp.archives		frp-archives@rpi.edu
rec.games.mud.announce		rgm-announce@glia.biostr.washington.edu
rec.guns			magnum@flubber.cs.umd.edu
rec.humor.funny			funny@clarinet.com
rec.humor.oracle		oracle-mod@cs.indiana.edu
rec.hunting			hunting@osnome.che.wisc.edu
rec.mag.fsfnet			white@duvm.bitnet
rec.music.gaffa			love-hounds@uunet.uu.net
rec.music.info			rec-music-info@ph.tn.tudelft.nl
rec.music.reviews		music_reviews@sco.com
rec.radio.broadcasting		rrb@airwaves.chi.il.us
rec.sport.cricket.scores	cricket@power.eee.ndsu.nodak.edu
sci.astro.hubble		sah@wfpc3.la.asu.edu
sci.math.research		sci-math-research@uiuc.edu
sci.med.aids			aids@cs.ucla.edu
sci.military			military@att.att.com
sci.nanotech			nanotech@aramis.rutgers.edu
sci.psychology.digest		psyc@phoenix.princeton.edu
sci.space.news			sci-space-news@news.arc.nasa.gov
sci.virtual-worlds		virtual-worlds@milton.u.washington.edu
soc.feminism			feminism@ncar.ucar.edu
soc.politics			poli-sci@rutgers.edu
soc.politics.arms-d		arms-d@xx.lcs.mit.edu
soc.religion.bahai		srb@oneworld.wa.com
soc.religion.christian		christian@aramis.rutgers.edu
soc.religion.eastern		sre@cse.ogi.edu
soc.religion.islam		religion-islam@ncar.ucar.edu
alt.atheism.moderated		atheism@mantis.co.uk
alt.binaries.pictures.fine-art.d	artcomp@uxa.ecn.bgu.edu
alt.binaries.pictures.fine-art.digitized	artcomp@uxa.ecn.bgu.edu
alt.binaries.pictures.fine-art.graphics	artcomp@uxa.ecn.bgu.edu
alt.comp.acad-freedom.news	caf-news@eff.org
alt.dev.null			/dev/null
alt.gourmand			recipes@decwrl.dec.com
alt.hackers			/dev/null
alt.hindu			editor@rbhatnagar.csm.uc.edu
alt.politics.democrats		news-submit@dc.clinton-gore.org
alt.politics.democrats.clinton	news-submit@dc.clinton-gore.org
alt.politics.democrats.governors	news-submit@dc.clinton-gore.org
alt.politics.democrats.house	news-submit@dc.clinton-gore.org
alt.politics.democrats.senate	news-submit@dc.clinton-gore.org
alt.security.index		kyle@uunet.uu.net
alt.society.ati			gzero@tronsbox.xei.com
alt.society.cu-digest		tk0jut2@mvs.cso.niu.edu
alt.sources.index		kyle@uunet.uu.net
austin.eff			eff-austin-moderator@tic.com
ba.announce			ba-announce@zorch.sf-bay.org
bionet.announce			biosci-announce-moderator@genbank.bio.net
bionet.biology.computational	comp-bio-moderator@genbank.bio.net
bionet.molbio.ddbj.updates	ddbj-updates@genbank.bio.net
bionet.molbio.embldatabank.updates	embl-updates@genbank.bio.net
bionet.molbio.genbank.updates	lear@genbank.bio.net
bionet.software.sources		software-sources@genbank.bio.net
bit.listserv.big-lan		big-req@suvm.acs.syr.edu
bit.listserv.edtech		21765EDT%MSU@CUNYVM.CUNY.EDU
bit.listserv.gaynet		gaynet@athena.mit.edu
bit.listserv.hellas		sda106@psuvm.psu.edu
bit.listserv.l-hcap		wtm@bunker.shel.isc-br.com
bit.listserv.libres		librk329@KentVMS.Kent.edu
bit.listserv.new-list		NU021172@VM1.NoDak.EDU
bit.listserv.pacs-l		LIBPACS%UHUPVM1@CUNYVM.CUNY.EDU
bit.listserv.valert-l		krvw@cert.org
biz.dec.decnews			decnews@mr4dec.enet.dec.com
biz.sco.announce		scoannmod@xenitec.on.ca
biz.sco.binaries		sl@wimsey.bc.ca
biz.sco.sources			kd1hz@anomaly.sbs.risc.net
biz.zeos.announce		kgermann@zeos.com
can.canet.d			canet-d@canet.ca
can.uucp.maps			pathadmin@cs.toronto.edu
comp.protocols.iso.x400.gateway	ifip-gtwy-usenet@ics.uci.edu
comp.security.announce		cert@cert.org
ddn.mgt-bulletin		nic@nic.ddn.mil
ddn.newsletter			nic@nic.ddn.mil
de.admin.lists			de-admin-lists@hactar.hanse.de
de.admin.submaps		maps@flatlin.ka.sub.org
de.comp.sources.amiga		agnus@amylnd.stgt.sub.org
de.comp.sources.misc		sources@watzman.quest.sub.org
de.comp.sources.os9		fkk@stasys.sta.sub.org
de.comp.sources.st		sources-st@watzman.quest.sub.org
de.comp.sources.unix		de-comp-sources-unix@germany.sun.com
de.mag.chalisti			ccc@sol.ccc.de
de.newusers			newusers@jattmp.nbg.sub.org
de.org.dfn			org-dfn@dfn.de
de.org.eunet			news@germany.eu.net
de.org.sub			vorstand@smurf.sub.org
de.sci.ki			hein@damon.irf.uni-dortmund.de
de.sci.ki.mod.ki		hein@damon.irf.uni-dortmund.de
fj.announce			fj-announce@junet.ad.jp
fj.binaries			fj-binaries@junet.ad.jp
fj.binaries.x68000		fj-binaries-x68000@junet.ad.jp
fj.guide.admin			fj-guide-admin@junet.ad.jp
fj.guide.general		fj-guide-general@junet.ad.jp
fj.guide.newusers		fj-guide-newusers@junet.ad.jp
fj.map				fj-map@junet.ad.jp
gnu.announce			info-gnu@prep.ai.mit.edu
gnu.bash.bug			bug-bash@prep.ai.mit.edu
gnu.emacs.announce		info-gnu-emacs@prep.ai.mit.edu
gnu.emacs.bug			bug-gnu-emacs@prep.ai.mit.edu
gnu.g++.announce		info-g++@prep.ai.mit.edu
gnu.g++.bug			bug-g++@prep.ai.mit.edu
gnu.g++.lib.bug			bug-lib-g++@prep.ai.mit.edu
gnu.gcc.announce		info-gcc@prep.ai.mit.edu
gnu.gcc.bug			bug-gcc@prep.ai.mit.edu
gnu.gdb.bug			bug-gdb@prep.ai.mit.edu
gnu.ghostscript.bug		bug-ghostscript@prep.ai.mit.edu
gnu.groff.bug			bug-groff@prep.ai.mit.edu
gnu.smalltalk.bug		bug-gnu-smalltalk@prep.ai.mit.edu
gnu.utils.bug			bug-gnu-utils@prep.ai.mit.edu
houston.weather			weather-monitor@tmc.edu
ieee.tcos			tcos@cse.ucsc.edu
info.academic.freedom		caf-talk@eff.org
info.admin			usenet@ux1.cso.uiuc.edu
info.bind			bind@arpa.berkeley.edu
info.brl.cad			cad@brl.mil
info.bytecounters		bytecounters@venera.isi.edu
info.cmu.tek.tcp		cmu-tek-tcp@cs.cmu.edu
info.convex			info-convex@pemrac.space.swri.edu
info.firearms			firearms@cs.cmu.edu
info.firearms.politics		firearms-politics@cs.cmu.edu
info.gated			gated-people@devvax.tn.cornell.edu
info.ietf			ietf@venera.isi.edu
info.ietf.hosts			ietf-hosts@nnsc.nsf.net
info.ietf.isoc			isoc-interest@relay.sgi.com
info.ietf.njm			njm@merit.edu
info.ietf.smtp			ietf-smtp@dimacs.rutgers.edu
info.isode			isode@nic.ddn.mil
info.jethro.tull		jtull@remus.rutgers.edu
info.labmgr			labmgr@ukcc.uky.edu
info.mach			info-mach@cs.cmu.edu
info.mh.workers			mh-workers@ics.uci.edu
info.nets			info-nets@think.com
info.nsf.grants			grants@note.nsf.gov
info.nsfnet.cert		nsfnet-cert@merit.edu
info.nysersnmp			nysersnmp@nisc.nyser.net
info.osf			roma@uiuc.edu
info.pem.dev			pem-dev@tis.com
info.ph				info-ph@uxc.cso.uiuc.edu
info.rfc			rfc-request@nic.ddn.mil
info.snmp			snmp@nisc.nyser.net
info.sun.managers		sun-managers@rice.edu
info.sun.nets			sun-nets@umiacs.umd.edu
info.theorynt			theorynt@vm1.nodak.edu
info.unix.sw			unix-sw-request@wsmr-simtel20.army.mil
mi.map				uucpmap@rel.mi.org
opinions.supreme-court		opinions@uunet.uu.net
relcom.infomarket.quote		relcom-infomarket-quote@news.ussr.eu.net
relcom.infomarket.talk		relcom-infomarket-talk@news.ussr.eu.net
relcom.jusinf			relcom-jusinf@news.ussr.eu.net
relcom.postmasters		relcom-postmasters@news.ussr.eu.net
relcom.renews			relcom-renews@news.ussr.eu.net
resif.oracle			oracle@grasp1.univ-lyon1.fr
sfnet.atk.flpf.tiedotukset	flpf@nic.funet.fi
sfnet.csc.tiedotukset		netmgr@csc.fi
sfnet.funet.tiedotukset		toimitus@funet.fi
sfnet.fuug.tiedotukset		sfnet-fuug-tiedotukset@fuug.fi
sfnet.harrastus.astronomia	pvtmakela@cc.helsinki.fi
sfnet.harrastus.mensa		jau@cs.tut.fi
sfnet.lists.sunflash		flash@sunvice.East.Sun.COM
sfnet.opiskelu.ymp.kurssit	hoffren@cc.Helsinki.FI
sfnet.tiede.tilastotiede.jatkokoulutus	til_tied@cc.helsinki.fi
sura.announce			sura-announce@darwin.sura.net
sura.noc.status			sura-noc-status@darwin.sura.net
sura.security			sura-security@darwin.sura.net
tamu.religion.christian		shetler@eemips.tamu.edu
tx-thenet-managers		themgr-moderator@nic.the.net
tx.maps				texas-uucpmaps@tmc.edu
uiuc.org.men			uiuc-men-ml@ux1.cso.uiuc.edu
uunet.alternet			asp@uunet.uu.net,postman@uunet.uu.net
uunet.announce			postman@uunet.uu.net
uunet.products			postman@uunet.uu.net
uunet.status			postman@uunet.uu.net
uunet.tech			postman@uunet.uu.net
vmsnet.announce			vmsnet-announce@mccall.com
vmsnet.announce.newusers	vmsnet-announce-newusers@mccall.com
vmsnet.sources			vmsnet-sources@mvb.saic.com
$eod 
