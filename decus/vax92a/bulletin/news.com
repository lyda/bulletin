$set nover
$copy/log sys$input NEWS.CREATE
$deck

From: lear@turbo.bio.net
Date: 25-JUL-1991 23:16:18
Description: How to Create a New Newsgroup

Original-from: woods@ncar.ucar.edu (Greg Woods)
[Most recent change: 19 May 1991 by tale@rpi.edu (David C Lawrence)]

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
   to announce-newgroups@rpi.edu.

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
   send a message to group-advice@rpi.edu; a few seasoned news administrators
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
   vote tally and the E-mail addresses and (if available) names of the votes
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
   Gene Spafford <spaf@cs.purdue.edu> and David C. Lawrence <tale@rpi.edu> with
   both the moderator's contact address and the group's submission address.

5) A proposal which has failed under point (3) above should not again be
   brought up for discussion until at least six months have passed from the
   close of the vote.  This limitation does not apply to proposals which never
   went to vote.


$eod
$copy/log sys$input NEWS.TRIAL
$deck

From: brad@looking.on.ca
Date: 25-JUL-1991 23:16:19
Description: How to Create a New Trial Newsgroup

Original-from: brad@looking.on.ca (Brad Templeton)
[Most recent change: 04 Oct 1990 by brad@looking.on.ca (Brad Templeton)]

      GUIDELINES FOR USENET GROUP CREATION (Trial Method)

(Note: This note describes a way of creating newsgroups that is
somewhat different from the generally-accepted proposal/vote method.
This method has not been universally accepted as a valid means of
creating a group, nor do all sites carry the "trial" hierarchy.
Groups created under this procedure may or may not be honored by all
site administrators, and may not be listed in the monthly newsgroup
list postings despite "approval" by this method. --spaf)


To create a group on USENET, you must objectively demonstrate to most
USENET admins that the group under consideration is worth carrying on
their machines, and thus by default to all of USENET.

One common method of performing this demonstration is to conduct a
survey/vote.  Another regular posting in this group describes that method.

Another method is to give the group a trial run in a smaller section of the
to see how it does.  This is described below.

(These are just guidelines.  Other methods exist, and these guidelines
have been written to be flexible.  The real goal is that sentence
above -- an objective demonstation that it's worth feeding the group,
by default, to all of the many thousands of machines on USENET.  What
you see below is just one way that many people think is a good way of
doing that.)

The trial.* hierarchy exists for new newsgroups that are being tried out.
A new group can be created there, and read by readers of that subnet.  The
readership and other forms of response are then measured.  At the end of
the trial (up to 5 months) the readership of the group is evaluated, and it
is calculated where it would fit into the whole of USENET.  If it meets the
criterion, it is moved into the USENET mainstream.  If it doesn't it is
simply deleted with a few weeks notice.  Readers of the group may elect to
form a mailing list or find another method of distribution.

	JUDGING THE SUITABILITY OF THE TOPIC

If you wish to create a group, you should ensure that you have a topic that
is likely to experience varied discussion for a long and indefinite period
of time.   The purpose of the group should be clear -- not too general
(like, "the IBM PC") and not too specific (like, "squid recipes").  The
topic should not be short lived, unless all you want is a 5 month run in
the trial hierarchy.   A general topic should be broken down until you have
something that is important and likely to generate a moderate volume of
discussion.

There are exceptions to this which you can only learn fully through
experience with USENET.

Next check to see if there isn't already a group on USENET that covers your
topic.  If you find a close match, read that group for a while to see if
your topic gets discussed there.  Bring it up yourself it you don't see it
discussed for a while.  (This is a strong requirement.  You should not
propose a new group if you have no familiarity with groups that might well
already contain discussion of your topic.)

If it turns out your topic is an offshoot of an existing group, and it is
in fact already heavily discussed in that group, you may wish to split that
group.  The trial group system has no mechanism for splitting regular USENET
groups.  You must arrange another means to do that.

If you have a truly new topic and:

	a) It is either different enough from all the other group topics that
	it needs its own newsgroup, or

	b) There is a group related to your topic, but the relationship is
	marginal, and the volume of discussion your topic would engender is
	too high for that group;

then you may indeed have a suitable topic for a new newsgroup.

Make sure as well that the topic is appropriate for world wide distribution.
You're going to be sending this discussion over more than 15,000 machines.
(In part, this is what the trial will decide if you aren't sure yourself.)

	STARTING THE TRIAL

Write up a proposal, listing:

	o) The proposed topic for the newsgroup.  If it's an obscure topic,
	   provide a bit of information about the nature of it.

	o) What sort of discussion you expect to see there.

	o) What hierarchy on USENET you think it might belong in.

	o) A suggestion as to possible names.

	o) Reasons why this topic really doesn't belong in other groups.

	o) Whether or not you plan to moderate a group.  (You can't suggest
	  a moderated group unless you have a moderator ready.)

(See other postings to trial.newgroups for a model proposal.)

The name and USENET hierarchy won't be chosen by you, but you can make
suggestions.  A good suggestion that is consistent with other existing
names is more likely to be chosen.  Note that a good name must be
meaningful to outsiders who are unfamiliar with the topic, so stay away
from acronyms or terms known only to insiders.  Unless you want to be
ultra-specific about what takes place in the group, a well understood name
is better than an extremely precise one.

E-mail the proposal to trialgroup@uunet.uu.net.  This will send it to all
of the volunteer trial group moderators, or "judges."  In addition, the
software will randomly pick one of these people to be the judge for your
trial group.  The judge will be a person with long experience with USENET,
and he or she will offer you help if there is room for improvement in your
proposal.  Some further names may also be suggested.  Joke proposals will
not be accepted -- a trial newsgroup involves a fair bit of effort and
expense by a lot of people, and no matter how funny a joke is, it won't be
as long lived as the effort in running a trial group.

[ Don't use the 'trialgroup' address other than to get a judge assigned
to your group.  That judge will reply to you -- correspond with him or
her directly thereafter.  If you really *have* to mail to all the trial
judges, and not the one(s) involved in your group, you can mail to
trial-judges@uunet.uu.net. ]

Once a trial name has been decided upon and the proposal finalized, the
trial group will be created by the trial hierarchy judges.  You should
then post the finalized proposal to news.announce.newgroups (or mail it
to the moderator at announce-newgroups@ncar.ucar.edu) and trial.newgroups.
(Due to the nature of moderated groups, you can't crosspost; you must post
twice, once to each group.)   If there is a group with a related topic,
you should also post the announcement of the new group there.

Now start using your group.  Write a more detailed description of the
group, and post it there.  Welcome new users and start discussion.  While
(unless you're moderator) you won't own or control the group after creating
it, you might see fit to act as a sort of custodian for the group, helping
new users, preparing lists of frequently asked questions or an introduction
to the group that gets posted every month or two.  If you stop reading your
group sometime in the future, you should find another volunteer for that
position.

Many sites in the trial hierarchy will send in readership reports.  If your
site does not do this, look for the "arbitron" program and instructions
posted at the start of every month in news.admin, and get your site
involved.   The arbitron results will be posted on the 1st of every month.

For the first 1-2 months, the results for your group will be artificiallyC
low, since it takes time for results to come in from a wide enough range ofi
sites.  Don't be concerned about those early figures.w

After the 5th readership report to include your group, the day of judgemente
is at hand.   All USENET groups, and your group, will be ranked accordingU
to the number of readers per site which gets the group.  (Your group willr
only go to the trial subnet, so both numbers will be reduced, keeping then
ratio valid.)  "USENET groups" means all the accepted groups in thee
7 main hierarchies of USENET, less those that have been truly dormant foru
several months, in the judge's opinion.t

If your group ranks among the top 3/4 of USENET groups in readers per site,f
it gets promoted to a USENET group.   Right now (May/90) this means ad
readership value of about 1 reader for every 3 sites getting the group, but 
that may change as USENET grows.  If your group can't find a reader on 2/3
of the sites it goes to, it's probably not an appropriate topic fora
full-net distribution.

Either way, pass or fail, mail a reminder of the results to your judge.

FAILURE:

An announcement will be made (by you, or failing that, a trial hierarchy
judge) indicating that the group failed the test.  This gives you time
to wrap up affairs, or consider the creation of a mailing list devoted toi
the topic.  After 2-3 weeks the group will be deleted (rmgrouped).  Any 
attempts to post to it will result in mail to the poster and the USENET 
admin at the poster's site.e

SUCCESS:

If the group passes the test, it gets renamed.  That means that a USENET
group with an appropriate name will be created.  Many sites will alias the
old name to the new name.  You should tell all readers of the trial group 
about the new name, and get them to switch over.  You may crosspost betweent
the two groups for the first week, but after that, you should actively
discourage any crossposting between the groups.  After a few more weeks,
the trial group will be deleted, with a few days notice.

Post once again to news.announce.newgroups, indicating that the group passed
and has now been created within USENET proper.

Do the same thing in the USENET group that you did in the trial group.
Tell people about the group, and post any standard introductory postings
that you may have written.  Welcome the new readers.  Then participate in.
the group.

	FAST PROMOTION:

If a group ever ranks in the top 50% of USENET groups in readers/site, it 
can, at your discretion, move over to USENET immediately.  Mail to
your judge and request the immediate move.   This can happen at any 
time, but the group must get this ranking in the top 50% with 
results from at least half the trial hierarchy.  (ie. if you only get 6a
sites reporting in the first month, and they all have 10 readers, it
doesn't qualify.)t

	REFUSAL OF A TRIALs

In rather unusual cases, the judges can refuse a serious trial.  For
example, an illegal group might cause this to happen.  If the judges
can't convince the proponent of the group that it's not a good idea, theye
may decide to register opposition to the trial.   At the start of thes
trial -- prior to the group's creation, a vote from 2/3 of the judges
(5 of the 7) can refuse the trial.   At the end of the trial, a vote
from 3/4 of the judges (6 of the 7) can stop the group's promotion.d

Is this a horrible autocratic power?  Not at all.  If a trial or
promotion is refused by the above veto, the proponent of the group
is still free to use the old discussion/survey method of group creation --
ie. things fall back to the method we had before the trial hierarchy was
created.  Refusing a trial is a very serious move that judges will
do only very rarely -- not simply because they don't like a group, but
rather because they feel it could cause serious damage to the net.  To
get 5 or 6 judges to agree to a refusal will mean there's a real
problem, indeed.


	NAMING 

As described above, a trial name will be decided by the trial judges ine
conjunction with you, the group's "champion."  They get the final say.  If
there is real feeling that the name is inappropriate, you can discuss this
in your trial group.  Proposals for a new name can then be put to theo
trial judges.  This may result in a new name if the group is promoted to
full distribution.   The final decision, however, remains up to the trial
judges.  If they really miss the boat on a couple of names, they won't
be trusted as trial judges for much longer.a

	HIERARCHY

You can suggest a hierarchy to the trial judges, but the decision is upu
to them.  Here are the existing hierarchies:

COMP
	Computer related technical discussion.  In general, groups in
	COMP are expected to have direct value to a site's commercial ord
	academic goals.

SCI)
	Groups about scientific topics, again expected to have direct
	value to a site's commercial or academic goals.

RECu
	Groups about recreation, hobbies, sports, entertainment, leisure,
	the arts, etc.r

NEWS
	Groups pertaining to USENET and USENET related networking. 

SOC 
	Groups about social issues and social interaction.  The
	humanities, etc. (excluding the recreational arts.)

TALK
	Groups about topics that often engender heated or emotional
	debate.  Politics, religion, abortion, philosophy, text editors,h
	discrimination etc.  Note that even if a topic seems a perfect
	fit for another hierarchy, if it's going to experience a lot of
	heated debate and "flaming," then it belongs here.  "Social club"
	groups, which exist more to talk to friends than to address a
	specific topic, also belong here.

MISC
	The rest, including most business related topics.

ALTn
	You don't need a trial to start a group in ALT.  Go ahead.t
	But the more people "go ahead" without following the guidelines
	suggested above (or in the other group creation guidelines) the
	more sites that will ignore ALT groups.

BIZ 
	The "biz" hierarchy is not part of USENET.  The trial.biz
	hierarchy, however, exists for the creation of commerce related
	groups.  Some people love these groups (misc.jobs.offered is oned
	of the most popular on USENET) and some people think they have no
	place here.  "trial.biz" is a place to try out such groups.  We'reo
	not talking about groups with nothing but hype, but useful groups
	(like misc.jobs.offered and misc.forsale) that benefit both the
	posters and the readers.   When moved into USENET they will
	be put into another hierarchy (usually MISC or COMP) dependings
	on what's appropriate.  If you want to use the top level BIZ 
	hierarchy, there are no hard and fast rules.  Post to biz.config.

GNU,VMSNET,UNIX-PC,BIONET,CLARI,etc.
	These hierarchies are not part of mainstream USENET.  They have
	their own rules for group creation which you must investigate.o

	NOTES: 

Risky business:

As noted, any serious group proposal will be created as a trial.  You mays
get some advice not to do so from the trial group judges, but if you
insist, it will be done.  Some sites, however, will refuse to carryl
quasi-illegal material, such as groups relating to sex, drugs, porn ande
other activities that are illegal or discouraged in some parts of the net.
Their machines are theirs to command, so it is considered polite to warn
everybody about a group with potential danger so that they can make theire
own decisions.

Appeals:

If your group fails, it is advised that you not suggest it again for
at least a year, unless something really unusual comes up.  It would
take very rare circumstances for the trial judges to restart a trial
in less than six months.

You can, after a trial fails, go through another group creation process,
such as a discussion/survey.  Nobody knows what will happen the first time
somebody tries this.  I predict that people won't take kindly to this, but
who knows.  (Some people may hate the trial system and support you only fore
that reason.)h

Autonomous admins:

Even after your group has a good trial, news administrators on their
own machines are not bound to create, carry or propagate your group.
The fact that it does well makes it pretty likely, but not assured,
particularly at the leafs.  This is true in any group creation system.

Cheating:A

Deliberate attempts to bias the arbitron statistics are likely to getb
discovered.  If you're caught and exposed, it's likely to do you more harm thane
good.  If there's proof, the judges will cancel your trial, and even if 
there isn't it is likely that you will anger site admins enough that theyr
ignore the group even if it appears to pass the test.  A suspect passing
result can be worse than a failed one -- so don't even risk it.e

Running around making sure reader sites send in their honest readershipl
reports will bias the results somewhat.  This is accounted for, and in the
long run, it's good, because those reports will keep coming, and they report
on all trial groups, not just yours.

The Judges:

The rules above are deliberately vague to give the judges room to breathe,
and to stop control freaks from pointing out nitpicking technicalt
violations.  A technical violation of this procedure is tough, because
things are left open.i

If you don't like the judges and how they rule, you can always use the
discussion/survey group creation method.

The only rules the judges must follow are:
	o) The current success/fail criteria must be announced in advance.g
	o) If there's a conflict of interest, a judge dealing with
	   a particular group should pass the decision on to anotheru
	   judge.
	o) Decisions can be appealed to a tribunal, where each of the
	   3 judges will write a public decision, majority winning.
	o) All serious proposals that haven't been done recently should be
	   accepted if the group's champion insists, but judges are freed
	   to put a dissenting comment on any such proposal.l
	o) A tribunal is selected randomly from the pool of judges, excepting
	   the judge being appealed.  No appeal on a tribunal's decision.

Other "good ideas" are: 
	o) Decisions about the final name should be done by a tribunal,
	   where possible, based on input from the trial group and e-mailed
	   comments from the net at large.a
	o) No simultaneous creation by both methods.  Users who do both
	   may get their trial group summarily deleted for annoying the
	   judges and giving them extra work to make the user's life easier.,


Who are the judges?h

There are 7.  I, Brad Templeton, am the first "chief justice."  The,
chief justice has no special powers, other than having written this document,w
but hey, the title sounds neat.  The other 6 volunteers are all system
admins, from a variety of areas on USENET, who have had several yearsI
experience with USENET and USENET groups. 

All judges will pass on any decision to other volunteer judges
if they have a conflict of interest.  (ie. plan to read the group, orm
participate actively in a similar group.)a

The judges really aren't all that powerful.  The *real* decision as to
whether a group gets created belongs with the readers.  If they like the
group, it stays, if they don't, it goes.   The judges only get final
say on the name, and they get to interpret the minor guidelines.

Their primary goal is to give advice, as experienced netters, on how
to make a group creation go smoothly.  They aren't there to hinder the
process, or fight against it.  They get the title "judge" because it soundss
important, and it means that people will avoid arguing with them over silly 
nitpicky points.


$eod
