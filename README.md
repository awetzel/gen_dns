# Overview
**SEO.io** is an automatic domain hosting and content management service.  Let's see what we're talking about with a nice use case...

# Use Case 1: BuenaCarta wants 20 sites

![Architecture](https://github.com/inaka/seo.io/raw/master/architecture.png)

## Actors

### _BCO_
  BuenaCarta's owner
### _WRx_
  Writer x
### _EDx_
  Editor x
### _USR_
  An annonymous web user
### _SEO_
  _SEO_.io (a.k.a. The system)

## Story

  1.  _BCO_ asks _SEO_ to fill his 20 domains providing 200 keywords through _SEO_'s **Client-UI**
  2.  Given the keywords, _SEO_ uses its **Topic Generator** to create plausible article topics for each domain
  3.  _SEO_ pushes new topics to its **Article Queue**
  3.  _SEO_ uses its **Mailer** to send a mail to every writer telling him about the new needed articles
  4.  _WR1_ opens _SEO_'s **Writer-UI** and chooses an article topic to write about
  5.  _WR1_ uses _SEO_'s **Article-Editor** to write (or just paste) his new article [he can save as many drafts as he want, close and reopen the browser without loosing his data]
  6.  _WR1_ decides the article is done, and then submits it using _SEO_'s **Writer-UI**
  7.  _SEO_ uses its **Mailer** to send a mail to every editor telling him about the new submitted article
  8.  _ED1_ opens _SEO_'s **Editor-UI** and chooses an article to check
  9.  _ED1_ modifies the article using _SEO_'s **Article-Editor** [again: saving, closing, reopening, etc... is allowed]
  10. _ED1_ rejects the article and adds a reason for that on _SEO_'s **Editor-UI**
  11. _SEO_ uses its **Mailer** to send a mail to _WR1_ telling him about the rejection of his article
  12. _WR1_ opens _SEO_'s **Writer-UI** and, using the **Article-Editor**, corrects the article.  Then he submits it again
  13. _SEO_ uses its **Mailer** to send a mail to every editor telling him about the new submitted article
  14. _ED1_ opens the article in _SEO_'s **Editor-UI** and publishes it
  15. _SEO_ enables the corresponding domain on its **DNS Server**
  16. _SEO_ uses its **Mailer** to send a mail to _BCO_ telling him about the new published article
  17. _USR_ tries to reach the site using his browser
  18. _USR_'s browser queries _SEO_'s **DNS Server** for the domain
  19. _SEO_'s **DNS Server** returns the ip of one of its **Web Servers**
  20. _USR_'s browser navigates to the given ip
  21. _SEO_'s **Web Server** formats the corresponding article and returns it to _USR_

## Notes

  1.  _SEO_ should keep track of article history (with article status and contents)
  2.  On step 13... is it OK to notify every editor or should only _ED1_ be notified?
  3.  Of course, _USR_ may also be Google Crawler
  4.  We need a way to let _BCO_ pay for _SEO_'s services

## We may need

  * To give _BCO_ a way to check the status of his domains (with some kind of stats or something)
  * To generate stats or reports about the quality of _WRx_ articles (We can even check how many people read them)
  * To give _USR_ a chance to add comments
  * To let _BCO_ (or _WRx_? or _EDx_?) choose themes/skins for the domains/articles
