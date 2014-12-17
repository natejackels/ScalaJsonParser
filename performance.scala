object performance {
	def main(args: Array[String]): Unit = {
		var a = 0
		var startTime : Double = 0
		var endTime : Double = 0
		var runTime : Double = 0
		var totalTime: Double = 0
		var result2 = ""
		for (a <- 1 to 10002){
			println("TEST CASE NUMBER : "+a)
			startTime = java.lang.System.currentTimeMillis()
			var result = jsonParser.loads(
		"""[
  {
    "_id": "5491c1691612fd6c29ed0581",
    "index": 0,
    "guid": "4e00ed5a-75c2-413d-b556-ed89f1defc89",
    "isActive": true,
    "balance": "$2,620.37",
    "picture": "http://placehold.it/32x32",
    "age": 25,
    "eyeColor": "blue",
    "name": {
      "first": "Wilson",
      "last": "Holland"
    },
    "company": "QUILCH",
    "email": "wilson.holland@quilch.net",
    "phone": "+1 (964) 404-3975",
    "address": "849 Abbey Court, Ribera, Arizona, 6489",
    "about": "Culpa enim exercitation non amet quis labore. Consectetur deserunt excepteur Lorem minim reprehenderit anim occaecat et sit in culpa ex ex. Labore ex do esse cillum anim deserunt aliquip dolor velit excepteur. Velit duis duis ut quis esse non ex eu ad sit sunt adipisicing. Do excepteur nisi id cillum consequat.\r\n",
    "registered": "Monday, March 31, 2014 6:38 PM",
    "latitude": -86.001057,
    "longitude": 177.588155,
    "tags": [
      "nostrud",
      "tempor",
      "veniam",
      "non",
      "ut",
      "irure",
      "laboris"
    ],
    "range": [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9
    ],
    "friends": [
      {
        "id": 0,
        "name": "Helena Glass"
      },
      {
        "id": 1,
        "name": "Letitia Pennington"
      },
      {
        "id": 2,
        "name": "Page Dillard"
      }
    ],
    "greeting": "Hello, Wilson! You have 9 unread messages.",
    "favoriteFruit": "apple"
  },
  {
    "_id": "5491c169d80957b4687c6c1f",
    "index": 1,
    "guid": "6067bafe-eb84-4c65-b600-96e91d2d2eeb",
    "isActive": false,
    "balance": "$2,465.55",
    "picture": "http://placehold.it/32x32",
    "age": 37,
    "eyeColor": "brown",
    "name": {
      "first": "Payne",
      "last": "Morin"
    },
    "company": "AFFLUEX",
    "email": "payne.morin@affluex.name",
    "phone": "+1 (972) 502-3028",
    "address": "677 Veterans Avenue, Wescosville, Kansas, 9509",
    "about": "Voluptate laborum dolor laborum proident laboris nostrud tempor aliquip culpa. Magna ad tempor excepteur sit minim. Commodo exercitation aute amet duis magna velit dolore irure dolore. Fugiat laborum enim et ullamco cillum exercitation. Do pariatur officia ut officia ea elit eu incididunt sunt.\r\n",
    "registered": "Wednesday, October 29, 2014 5:16 AM",
    "latitude": 37.221471,
    "longitude": 63.11171,
    "tags": [
      "culpa",
      "sunt",
      "velit",
      "et",
      "non",
      "duis",
      "anim"
    ],
    "range": [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9
    ],
    "friends": [
      {
        "id": 0,
        "name": "Leblanc Weber"
      },
      {
        "id": 1,
        "name": "Margret Mcgowan"
      },
      {
        "id": 2,
        "name": "Myers Sosa"
      }
    ],
    "greeting": "Hello, Payne! You have 10 unread messages.",
    "favoriteFruit": "banana"
  },
  {
    "_id": "5491c1694d65f8b61395177d",
    "index": 2,
    "guid": "1d3cb327-d984-4036-840d-c0ae11c60011",
    "isActive": false,
    "balance": "$3,876.50",
    "picture": "http://placehold.it/32x32",
    "age": 30,
    "eyeColor": "green",
    "name": {
      "first": "Leonard",
      "last": "Nicholson"
    },
    "company": "TERRAGEN",
    "email": "leonard.nicholson@terragen.tv",
    "phone": "+1 (821) 564-3637",
    "address": "384 Conduit Boulevard, Greensburg, Marshall Islands, 1898",
    "about": "Id ullamco commodo Lorem eu nostrud esse. Mollit labore dolor esse minim commodo tempor officia ea labore sit. Aliquip excepteur occaecat voluptate tempor non duis sit.\r\n",
    "registered": "Saturday, September 27, 2014 8:36 AM",
    "latitude": 2.33639,
    "longitude": 6.245108,
    "tags": [
      "nisi",
      "ullamco",
      "est",
      "duis",
      "enim",
      "eiusmod",
      "culpa"
    ],
    "range": [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9
    ],
    "friends": [
      {
        "id": 0,
        "name": "Stevenson Torres"
      },
      {
        "id": 1,
        "name": "Velez French"
      },
      {
        "id": 2,
        "name": "Latonya Morales"
      }
    ],
    "greeting": "Hello, Leonard! You have 9 unread messages.",
    "favoriteFruit": "banana"
  },
  {
    "_id": "5491c169f35f52ff9477034e",
    "index": 3,
    "guid": "36f4e3ee-a16e-4058-a02e-31e39b277d0a",
    "isActive": true,
    "balance": "$2,567.13",
    "picture": "http://placehold.it/32x32",
    "age": 35,
    "eyeColor": "green",
    "name": {
      "first": "Ilene",
      "last": "Davis"
    },
    "company": "ZENOLUX",
    "email": "ilene.davis@zenolux.io",
    "phone": "+1 (841) 518-3446",
    "address": "137 Ridge Boulevard, Thynedale, Massachusetts, 7922",
    "about": "Amet mollit minim magna incididunt. Reprehenderit ullamco consequat labore qui sit sunt proident quis aliqua occaecat occaecat. Velit labore consequat excepteur magna occaecat et magna et occaecat minim nostrud labore mollit tempor. Enim anim ut proident ad aliqua commodo.\r\n",
    "registered": "Monday, October 20, 2014 1:14 AM",
    "latitude": -13.516096,
    "longitude": -139.965486,
    "tags": [
      "duis",
      "velit",
      "anim",
      "laborum",
      "nisi",
      "nulla",
      "tempor"
    ],
    "range": [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9
    ],
    "friends": [
      {
        "id": 0,
        "name": "Carver Ewing"
      },
      {
        "id": 1,
        "name": "Boyd Woodward"
      },
      {
        "id": 2,
        "name": "Aguilar Koch"
      }
    ],
    "greeting": "Hello, Ilene! You have 8 unread messages.",
    "favoriteFruit": "strawberry"
  },
  {
    "_id": "5491c169ffb5e31b60825135",
    "index": 4,
    "guid": "f9935912-20bd-44d7-95d2-fdc8ec281541",
    "isActive": true,
    "balance": "$2,788.18",
    "picture": "http://placehold.it/32x32",
    "age": 26,
    "eyeColor": "green",
    "name": {
      "first": "Jenny",
      "last": "Allen"
    },
    "company": "QUAREX",
    "email": "jenny.allen@quarex.me",
    "phone": "+1 (839) 537-3444",
    "address": "761 Dunham Place, Lisco, Montana, 2674",
    "about": "Et nulla elit deserunt proident dolor. Cillum Lorem incididunt enim non nostrud commodo elit in. Do proident nostrud officia aliqua dolor laborum esse. Aliqua id veniam nostrud sint aliquip ex anim quis aliqua. Dolor sunt qui et quis Lorem incididunt est. Pariatur proident consectetur reprehenderit consectetur exercitation cupidatat consectetur reprehenderit fugiat qui cupidatat elit. Et est consequat excepteur ad reprehenderit ad dolore.\r\n",
    "registered": "Sunday, June 22, 2014 4:20 PM",
    "latitude": 28.424332,
    "longitude": -159.114082,
    "tags": [
      "consectetur",
      "est",
      "commodo",
      "excepteur",
      "dolore",
      "ut",
      "sint"
    ],
    "range": [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9
    ],
    "friends": [
      {
        "id": 0,
        "name": "Farmer Alexander"
      },
      {
        "id": 1,
        "name": "Shanna Adams"
      },
      {
        "id": 2,
        "name": "Mayra Coleman"
      }
    ],
    "greeting": "Hello, Jenny! You have 9 unread messages.",
    "favoriteFruit": "banana"
  },
  {
    "_id": "5491c16954ac65b9014a8929",
    "index": 5,
    "guid": "2922d70e-71d0-4423-a967-a25cb2b88cb9",
    "isActive": false,
    "balance": "$1,582.49",
    "picture": "http://placehold.it/32x32",
    "age": 40,
    "eyeColor": "green",
    "name": {
      "first": "Stark",
      "last": "Meadows"
    },
    "company": "PLAYCE",
    "email": "stark.meadows@playce.com",
    "phone": "+1 (962) 433-2195",
    "address": "313 Garfield Place, Herlong, Virgin Islands, 9188",
    "about": "Qui cillum et nostrud laborum mollit. Proident ut aliqua adipisicing anim occaecat nulla sit ea deserunt esse consequat cupidatat. Nostrud magna irure ad Lorem quis Lorem voluptate laboris irure incididunt. Consequat commodo Lorem in non nisi nisi deserunt quis incididunt minim sint. Non culpa aliqua dolor mollit veniam. Ut magna consequat esse eu proident. Fugiat Lorem veniam et cillum ex duis.\r\n",
    "registered": "Sunday, August 17, 2014 9:14 AM",
    "latitude": 59.133684,
    "longitude": 7.61657,
    "tags": [
      "laborum",
      "do",
      "reprehenderit",
      "culpa",
      "quis",
      "eiusmod",
      "quis"
    ],
    "range": [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9
    ],
    "friends": [
      {
        "id": 0,
        "name": "Kristi White"
      },
      {
        "id": 1,
        "name": "Hooper Burnett"
      },
      {
        "id": 2,
        "name": "Brooke Daniel"
      }
    ],
    "greeting": "Hello, Stark! You have 8 unread messages.",
    "favoriteFruit": "apple"
  },
  {
    "_id": "5491c1697ea86b28a68daca0",
    "index": 6,
    "guid": "93ded9e4-25c7-4678-8c04-f3c04798e19f",
    "isActive": true,
    "balance": "$3,604.97",
    "picture": "http://placehold.it/32x32",
    "age": 27,
    "eyeColor": "blue",
    "name": {
      "first": "Holt",
      "last": "Bond"
    },
    "company": "BITENDREX",
    "email": "holt.bond@bitendrex.biz",
    "phone": "+1 (966) 519-3750",
    "address": "298 Whitty Lane, Allendale, Nevada, 9915",
    "about": "Irure sunt mollit magna nulla irure irure dolor do consequat sunt laboris dolore. Cupidatat amet nisi ipsum Lorem magna elit et. Ipsum esse mollit irure dolore veniam pariatur veniam exercitation id deserunt occaecat aliqua.\r\n",
    "registered": "Wednesday, February 19, 2014 12:51 AM",
    "latitude": -8.271876,
    "longitude": -1.564051,
    "tags": [
      "officia",
      "quis",
      "veniam",
      "mollit",
      "proident",
      "est",
      "culpa"
    ],
    "range": [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9
    ],
    "friends": [
      {
        "id": 0,
        "name": "Sweeney Whitehead"
      },
      {
        "id": 1,
        "name": "Tucker Avila"
      },
      {
        "id": 2,
        "name": "Rush Lindsay"
      }
    ],
    "greeting": "Hello, Holt! You have 8 unread messages.",
    "favoriteFruit": "banana"
  }
]""")
	//		var result = jsonParser.loads("""{"kind": "Listing", "data": {"modhash": "", "children": [{"kind": "t3", "data": {"domain": "self.scala", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": "&lt;!-- SC_OFF --&gt;&lt;div class=\"md\"&gt;&lt;p&gt;I&amp;#39;ve tried several libraries and they seem to seriously lag behind libraries for other languages I&amp;#39;ve used.&lt;/p&gt;\n\n&lt;ol&gt;\n&lt;li&gt;&lt;p&gt;Anorm - unnecessary abstraction layer. Doesn&amp;#39;t do anything that couldn&amp;#39;t be done with a simple &lt;code&gt;map { row =&amp;gt; Item(row.getInt(0)) }&lt;/code&gt;&lt;/p&gt;&lt;/li&gt;\n&lt;li&gt;&lt;p&gt;Slick - no support for relations so after a join you essentially have to write another query to organize the result in a usable way. &lt;/p&gt;&lt;/li&gt;\n&lt;li&gt;&lt;p&gt;Squeryl - doesn&amp;#39;t support Scala 2.11, requires some changes to entity classes that aren&amp;#39;t really Scala-ish, weird query syntax, very poor support for relations.&lt;/p&gt;&lt;/li&gt;\n&lt;/ol&gt;\n\n&lt;p&gt;So, are Scala database access libraries really in such a sad state and I just have to soldier on or there&amp;#39;s something good that I&amp;#39;ve missed?&lt;/p&gt;\n&lt;/div&gt;&lt;!-- SC_ON --&gt;", "selftext": "I've tried several libraries and they seem to seriously lag behind libraries for other languages I've used.\n\n1. Anorm - unnecessary abstraction layer. Doesn't do anything that couldn't be done with a simple `map { row =&gt; Item(row.getInt(0)) }`\n\n2. Slick - no support for relations so after a join you essentially have to write another query to organize the result in a usable way. \n\n3. Squeryl - doesn't support Scala 2.11, requires some changes to entity classes that aren't really Scala-ish, weird query syntax, very poor support for relations.\n\nSo, are Scala database access libraries really in such a sad state and I just have to soldier on or there's something good that I've missed?", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2l58vw", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "pavlik_enemy", "media": null, "score": 2, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": true, "name": "t3_2l58vw", "permalink": "/r/scala/comments/2l58vw/what_do_you_use_for_rdbms_access/", "stickied": false, "created": 1415036268.0, "url": "http://www.reddit.com/r/scala/comments/2l58vw/what_do_you_use_for_rdbms_access/", "author_flair_text": null, "title": "What do you use for RDBMS access?", "created_utc": 1415007468.0, "ups": 2, "num_comments": 11, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "infoq.com", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": null, "selftext": "", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2l2379", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "predef", "media": null, "score": 16, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": false, "name": "t3_2l2379", "permalink": "/r/scala/comments/2l2379/jessica_kerr_on_java_vs_scala_property_based/", "stickied": false, "created": 1414957532.0, "url": "http://www.infoq.com/interviews/jessica-kerr-java-scala-testing", "author_flair_text": null, "title": "Jessica Kerr on Java vs. Scala, Property Based Testing, and Diversity in IT", "created_utc": 1414928732.0, "ups": 16, "num_comments": 8, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "scala-lang.org", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": null, "selftext": "", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2kwjat", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "didyoumean", "media": null, "score": 29, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": false, "name": "t3_2kwjat", "permalink": "/r/scala/comments/2kwjat/scala_2114_is_now_available/", "stickied": false, "created": 1414806262.0, "url": "http://scala-lang.org/news/2.11.4", "author_flair_text": null, "title": "Scala 2.11.4 is now available!", "created_utc": 1414777462.0, "ups": 29, "num_comments": 4, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "self.scala", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": "&lt;!-- SC_OFF --&gt;&lt;div class=\"md\"&gt;&lt;p&gt;I was running through some of the easy challenges on CodeEval, and I came across &lt;a href=\"https://www.codeeval.com/open_challenges/87/\"&gt;QueryBoard&lt;/a&gt;. I&amp;#39;ve been trying to grok the state monad since I watched Brian Beckman&amp;#39;s &lt;a href=\"https://www.youtube.com/watch?v=XxzzJiXHOJs\"&gt;video&lt;/a&gt; on it a few months ago, and this challenge seemed like a good learning opportunity, so I came up with &lt;a href=\"https://gist.github.com/ffxtian/7a29cf14ea201c35714f\"&gt;this: https://gist.github.com/ffxtian/7a29cf14ea201c35714f&lt;/a&gt;. I&amp;#39;d appreciate your comments/feedback. A few points on which I&amp;#39;ve already thought:&lt;/p&gt;\n\n&lt;ul&gt;\n&lt;li&gt;I know scalaz has a state monad implementation, and using it would provide traverseS so I wouldn&amp;#39;t have to manually map and fold -- codeeval doesn&amp;#39;t support it, so I had to use &amp;quot;mine&amp;quot;&lt;/li&gt;\n&lt;li&gt;&amp;quot;My&amp;quot; implementation isn&amp;#39;t really mine, it&amp;#39;s a Scala translation of the Haskell implementation from the Brian Beckman video&lt;/li&gt;\n&lt;li&gt;I know I&amp;#39;m committing all sorts of mortal sin by calling println from within the query functions. &lt;/li&gt;\n&lt;li&gt;I also know that as a result of the previous point, my method &lt;strong&gt;f&lt;/strong&gt; (which I&amp;#39;m using to fold over the input) is a bit infantile -- I&amp;#39;m working on an alternate implementation where the results are collected into a List[Option[Int]] and then printed after processing.&lt;/li&gt;\n&lt;/ul&gt;\n&lt;/div&gt;&lt;!-- SC_ON --&gt;", "selftext": "I was running through some of the easy challenges on CodeEval, and I came across [QueryBoard](https://www.codeeval.com/open_challenges/87/). I've been trying to grok the state monad since I watched Brian Beckman's [video](https://www.youtube.com/watch?v=XxzzJiXHOJs) on it a few months ago, and this challenge seemed like a good learning opportunity, so I came up with [this: https://gist.github.com/ffxtian/7a29cf14ea201c35714f](https://gist.github.com/ffxtian/7a29cf14ea201c35714f). I'd appreciate your comments/feedback. A few points on which I've already thought:\n\n* I know scalaz has a state monad implementation, and using it would provide traverseS so I wouldn't have to manually map and fold -- codeeval doesn't support it, so I had to use \"mine\"\n* \"My\" implementation isn't really mine, it's a Scala translation of the Haskell implementation from the Brian Beckman video\n* I know I'm committing all sorts of mortal sin by calling println from within the query functions. \n* I also know that as a result of the previous point, my method **f** (which I'm using to fold over the input) is a bit infantile -- I'm working on an alternate implementation where the results are collected into a List[Option[Int]] and then printed after processing.", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2kxpl4", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "ffxtian", "media": null, "score": 4, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": true, "name": "t3_2kxpl4", "permalink": "/r/scala/comments/2kxpl4/feedback_on_my_state_monad_implementationapp/", "stickied": false, "created": 1414828835.0, "url": "http://www.reddit.com/r/scala/comments/2kxpl4/feedback_on_my_state_monad_implementationapp/", "author_flair_text": null, "title": "Feedback on my State Monad Implementation/App?", "created_utc": 1414800035.0, "ups": 4, "num_comments": 0, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "scalakata.com", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": null, "selftext": "", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2kw2df", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "MasGui", "media": null, "score": 12, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": false, "name": "t3_2kw2df", "permalink": "/r/scala/comments/2kw2df/a_tour_of_scala_wip/", "stickied": false, "created": 1414797179.0, "url": "http://scalakata.com", "author_flair_text": null, "title": "A Tour of Scala (WIP)", "created_utc": 1414768379.0, "ups": 12, "num_comments": 5, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "reactive.xploregroup.be", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": null, "selftext": "", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2kv7av", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "SlevinBE", "media": null, "score": 8, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": false, "name": "t3_2kv7av", "permalink": "/r/scala/comments/2kv7av/slick_a_new_approach_to_database_access_in_scala/", "stickied": false, "created": 1414771249.0, "url": "http://reactive.xploregroup.be/blog/5/Slick:-A-new-approach-to-database-access-in-Scala", "author_flair_text": null, "title": "Slick: A new approach to database access in Scala", "created_utc": 1414742449.0, "ups": 8, "num_comments": 23, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "thread.gmane.org", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": null, "selftext": "", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2ktjkw", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "based2", "media": null, "score": 13, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": false, "name": "t3_2ktjkw", "permalink": "/r/scala/comments/2ktjkw/scala_2114_release_has_been_staged/", "stickied": false, "created": 1414732319.0, "url": "http://thread.gmane.org/gmane.comp.lang.scala.internals/23709", "author_flair_text": null, "title": "Scala 2.11.4 release has been staged", "created_utc": 1414703519.0, "ups": 13, "num_comments": 1, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "blog.scaloid.org", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": null, "selftext": "", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2ks78i", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "pocorall", "media": null, "score": 23, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": false, "name": "t3_2ks78i", "permalink": "/r/scala/comments/2ks78i/scaloid_36_is_released/", "stickied": false, "created": 1414707514.0, "url": "http://blog.scaloid.org/2014/10/scaloid-36-is-released.html", "author_flair_text": null, "title": "Scaloid 3.6 is released", "created_utc": 1414678714.0, "ups": 23, "num_comments": 1, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "finagle.github.io", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": null, "selftext": "", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2ktszy", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "caniszczyk", "media": null, "score": 3, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": false, "name": "t3_2ktszy", "permalink": "/r/scala/comments/2ktszy/new_finagle_example_using_opennlp/", "stickied": false, "created": 1414737240.0, "url": "https://finagle.github.io/blog/2014/10/30/new-and-upcoming-finagle-examples/", "author_flair_text": null, "title": "New Finagle Example using OpenNLP", "created_utc": 1414708440.0, "ups": 3, "num_comments": 0, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "self.scala", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": "&lt;!-- SC_OFF --&gt;&lt;div class=\"md\"&gt;&lt;p&gt;I installed activator using the minimal version, hence it should have auto-downloaded the newest possible scala/sbt versions.&lt;/p&gt;\n\n&lt;p&gt;When launching a given app (i tried &amp;quot;realtime-search&amp;quot;), I get an error, which google tells me is related to java 8 incompatibility (error while loading CharSequence, class file &amp;#39;/Library/Java/JavaVirtualMachines/jdk1.8.0_05.jdk/...CharSequence.class)&amp;#39; is broken.&amp;#39;).&lt;/p&gt;\n\n&lt;p&gt;Most pages about the subject tell you to downgrade the JVM to 1.7, but Activator includes a number of java8-generators, which tells me that java 8 support should be possible?!&lt;/p&gt;\n\n&lt;p&gt;Could please somebody tell me &lt;/p&gt;\n\n&lt;p&gt;1) how to use sbt/activator with an update-to-date jvm?&lt;/p&gt;\n\n&lt;p&gt;2) how it can be that scala/sbt is by default incompatible with java 8, although it has been out for months and although before release, there was lots of time to prepare for it?&lt;/p&gt;\n&lt;/div&gt;&lt;!-- SC_ON --&gt;", "selftext": "I installed activator using the minimal version, hence it should have auto-downloaded the newest possible scala/sbt versions.\n\nWhen launching a given app (i tried \"realtime-search\"), I get an error, which google tells me is related to java 8 incompatibility (error while loading CharSequence, class file '/Library/Java/JavaVirtualMachines/jdk1.8.0_05.jdk/...CharSequence.class)' is broken.').\n\nMost pages about the subject tell you to downgrade the JVM to 1.7, but Activator includes a number of java8-generators, which tells me that java 8 support should be possible?!\n\nCould please somebody tell me \n\n1) how to use sbt/activator with an update-to-date jvm?\n\n2) how it can be that scala/sbt is by default incompatible with java 8, although it has been out for months and although before release, there was lots of time to prepare for it?\n", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2kp6mg", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "ib84", "media": null, "score": 6, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": true, "name": "t3_2kp6mg", "permalink": "/r/scala/comments/2kp6mg/how_to_use_activatorsbt_with_java_8/", "stickied": false, "created": 1414636033.0, "url": "http://www.reddit.com/r/scala/comments/2kp6mg/how_to_use_activatorsbt_with_java_8/", "author_flair_text": null, "title": "How to use activator/sbt with java 8 ?", "created_utc": 1414607233.0, "ups": 6, "num_comments": 9, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "self.scala", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": "&lt;!-- SC_OFF --&gt;&lt;div class=\"md\"&gt;&lt;p&gt;I&amp;#39;m writing a web application with Play framework and need an authentication plugin that can handle OAuth providers. I&amp;#39;ve tried Secure Social but the amount of hand-coding compared to say Devise for Rails is insane and man this code looks ugly. Are there any well-documented and easy to use plugins?&lt;/p&gt;\n&lt;/div&gt;&lt;!-- SC_ON --&gt;", "selftext": "I'm writing a web application with Play framework and need an authentication plugin that can handle OAuth providers. I've tried Secure Social but the amount of hand-coding compared to say Devise for Rails is insane and man this code looks ugly. Are there any well-documented and easy to use plugins?", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2klryn", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "pavlik_enemy", "media": null, "score": 3, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": true, "name": "t3_2klryn", "permalink": "/r/scala/comments/2klryn/authentication_plugin_for_play/", "stickied": false, "created": 1414531839.0, "url": "http://www.reddit.com/r/scala/comments/2klryn/authentication_plugin_for_play/", "author_flair_text": null, "title": "Authentication plugin for Play", "created_utc": 1414528239.0, "ups": 3, "num_comments": 13, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "self.scala", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": "&lt;!-- SC_OFF --&gt;&lt;div class=\"md\"&gt;&lt;p&gt;That&amp;#39;s not a very constructive post, just venting my frustration.&lt;/p&gt;\n\n&lt;p&gt;I&amp;#39;ve decided to create a simple app using Play framework. All went relatively well until I tried to add Facebook authentication. The first Google link pointed me at Secure Social. Well, it requires programmer to supply a persistence layer but since Play doesn&amp;#39;t have bolted-on ORM it&amp;#39;s understandable so what the hell. It turns out to use it you have to turn your controllers into classes and implement a controller factory for no clear reason. At least this part is documented. Now, you can&amp;#39;t use default views because they don&amp;#39;t care about your master page/layout, but this part is not documented (even worse, the documentation is wrong) so I had to go through its source code. The task of changing the auth provider list required digging through sources as well as some coding because you can&amp;#39;t do it using configuration. &lt;/p&gt;\n\n&lt;p&gt;Now to Play itself. Facebook auth wasn&amp;#39;t working so naturally I had to check what Facebook sends me and how the app handles this stuff. Yeah, right, it turns out there&amp;#39;s a stupid-ass Activator UI but no functionality to view the requests handled by the application.&lt;/p&gt;\n\n&lt;p&gt;Overall experience was worse than coding in PHP, at least with PHP you don&amp;#39;t expect anything. Play is years behind proper web frameworks like Rails, I don&amp;#39;t really understand how people could use this stuff.&lt;/p&gt;\n&lt;/div&gt;&lt;!-- SC_ON --&gt;", "selftext": "That's not a very constructive post, just venting my frustration.\n\nI've decided to create a simple app using Play framework. All went relatively well until I tried to add Facebook authentication. The first Google link pointed me at Secure Social. Well, it requires programmer to supply a persistence layer but since Play doesn't have bolted-on ORM it's understandable so what the hell. It turns out to use it you have to turn your controllers into classes and implement a controller factory for no clear reason. At least this part is documented. Now, you can't use default views because they don't care about your master page/layout, but this part is not documented (even worse, the documentation is wrong) so I had to go through its source code. The task of changing the auth provider list required digging through sources as well as some coding because you can't do it using configuration. \n\nNow to Play itself. Facebook auth wasn't working so naturally I had to check what Facebook sends me and how the app handles this stuff. Yeah, right, it turns out there's a stupid-ass Activator UI but no functionality to view the requests handled by the application.\n\nOverall experience was worse than coding in PHP, at least with PHP you don't expect anything. Play is years behind proper web frameworks like Rails, I don't really understand how people could use this stuff.", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2knez2", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "pavlik_enemy", "media": null, "score": 0, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": true, "name": "t3_2knez2", "permalink": "/r/scala/comments/2knez2/scala_web_frameworks_are_made_and_used_by_people/", "stickied": false, "created": 1414564438.0, "url": "http://www.reddit.com/r/scala/comments/2knez2/scala_web_frameworks_are_made_and_used_by_people/", "author_flair_text": null, "title": "Scala web frameworks are made and used by people who never seen a web framework before", "created_utc": 1414560838.0, "ups": 0, "num_comments": 15, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "reactive.xploregroup.be", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": null, "selftext": "", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2khmq9", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "SlevinBE", "media": null, "score": 16, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": false, "name": "t3_2khmq9", "permalink": "/r/scala/comments/2khmq9/futures_and_thread_pools/", "stickied": false, "created": 1414465090.0, "url": "http://reactive.xploregroup.be/blog/7/Futures-and-Thread-pools", "author_flair_text": null, "title": "Futures and Thread pools", "created_utc": 1414436290.0, "ups": 16, "num_comments": 0, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "gfycat.com", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": null, "selftext": "", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2kg9fg", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "makemeunsee", "media": null, "score": 11, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": false, "name": "t3_2kg9fg", "permalink": "/r/scala/comments/2kg9fg/boids_flock_simulation_with_akka/", "stickied": false, "created": 1414433234.0, "url": "https://gfycat.com/FavorableCoarseAfricanharrierhawk", "author_flair_text": null, "title": "Boids / flock simulation with Akka", "created_utc": 1414404434.0, "ups": 11, "num_comments": 4, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "scalafx.org", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": null, "selftext": "", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2kd3qc", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "based2", "media": null, "score": 25, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": false, "name": "t3_2kd3qc", "permalink": "/r/scala/comments/2kd3qc/scalafx/", "stickied": false, "created": 1414328447.0, "url": "http://www.scalafx.org/", "author_flair_text": null, "title": "ScalaFX", "created_utc": 1414324847.0, "ups": 25, "num_comments": 3, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "self.scala", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": "&lt;!-- SC_OFF --&gt;&lt;div class=\"md\"&gt;&lt;p&gt;I&amp;#39;m going through &lt;a href=\"http://twitter.github.io/scala_school/basics.html\"&gt;Scala school basics&lt;/a&gt; and I noticed this:&lt;/p&gt;\n\n&lt;pre&gt;&lt;code&gt;def multiply(m: Int)(n: Int): Int = m * n\n&lt;/code&gt;&lt;/pre&gt;\n\n&lt;p&gt;What confuses me is &lt;code&gt;(m: Int)(n: Int)&lt;/code&gt; ... it seems more common for it to be\n&lt;code&gt;(m: Int, n: Int)&lt;/code&gt;  ... what exactly is the meaning of &lt;code&gt;(m: Int)(n: Int)&lt;/code&gt; and\nhow does it differ from &lt;code&gt;(m: Int, n: Int)&lt;/code&gt; ?&lt;/p&gt;\n&lt;/div&gt;&lt;!-- SC_ON --&gt;", "selftext": "I'm going through [Scala school basics](http://twitter.github.io/scala_school/basics.html) and I noticed this:\n\n    def multiply(m: Int)(n: Int): Int = m * n\n\nWhat confuses me is `(m: Int)(n: Int)` ... it seems more common for it to be\n`(m: Int, n: Int)`  ... what exactly is the meaning of `(m: Int)(n: Int)` and\nhow does it differ from `(m: Int, n: Int)` ?\n", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2k9wn1", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "metaperl", "media": null, "score": 16, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": true, "name": "t3_2k9wn1", "permalink": "/r/scala/comments/2k9wn1/def_multiplym_intn_int_int_m_n/", "stickied": false, "created": 1414233165.0, "url": "http://www.reddit.com/r/scala/comments/2k9wn1/def_multiplym_intn_int_int_m_n/", "author_flair_text": null, "title": "def multiply(m: Int)(n: Int): Int = m * n", "created_utc": 1414229565.0, "ups": 16, "num_comments": 16, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "rawcoders.com", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": null, "selftext": "", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2ka61o", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "Psycho_Coder", "media": null, "score": 5, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": false, "name": "t3_2ka61o", "permalink": "/r/scala/comments/2ka61o/scala_lempelzivwelch_text_compression/", "stickied": false, "created": 1414270007.0, "url": "http://www.rawcoders.com/Thread-Source-Scala-Lempel-Ziv-Welch-Text-Compression#.VEubA6oxwz8.reddit", "author_flair_text": null, "title": "[Scala] Lempel-Ziv-Welch Text Compression", "created_utc": 1414241207.0, "ups": 5, "num_comments": 0, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "self.scala", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": "&lt;!-- SC_OFF --&gt;&lt;div class=\"md\"&gt;&lt;p&gt;I&amp;#39;m reading &lt;a href=\"http://twitter.github.io/scala_school/basics.html\"&gt;Scala school&lt;/a&gt; and am wondering how these two statements differ:&lt;/p&gt;\n\n&lt;pre&gt;&lt;code&gt;val addOne = (x: Int) =&amp;gt; x + 1\n\ndef addOne(m: Int): Int = m + 1\n&lt;/code&gt;&lt;/pre&gt;\n&lt;/div&gt;&lt;!-- SC_ON --&gt;", "selftext": "I'm reading [Scala school](http://twitter.github.io/scala_school/basics.html) and am wondering how these two statements differ:\n\n    val addOne = (x: Int) =&gt; x + 1\n\n    def addOne(m: Int): Int = m + 1", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2k70k6", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "metaperl", "media": null, "score": 11, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": true, "name": "t3_2k70k6", "permalink": "/r/scala/comments/2k70k6/whats_the_difference_between_def_addone_and_val/", "stickied": false, "created": 1414161299.0, "url": "http://www.reddit.com/r/scala/comments/2k70k6/whats_the_difference_between_def_addone_and_val/", "author_flair_text": null, "title": "What's the difference between def addOne and val addOne?", "created_utc": 1414157699.0, "ups": 11, "num_comments": 8, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "self.scala", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": "&lt;!-- SC_OFF --&gt;&lt;div class=\"md\"&gt;&lt;p&gt;Hello, how would I get the value of the defined function &lt;code&gt;addOne&lt;/code&gt; in the session below? simply typing &lt;code&gt;addOne&lt;/code&gt; into the console did not return the value of the function, while typing &lt;code&gt;res1&lt;/code&gt; returned the value of the anonymous function.&lt;/p&gt;\n\n&lt;pre&gt;&lt;code&gt;scala&amp;gt; def addOne(m: Int): Int = m + 1\naddOne: (m: Int)Int\n\nscala&amp;gt; val three = addOne(2)\nthree: Int = 3\n\nscala&amp;gt; (x: Int) =&amp;gt; x + 1\nres1: Int =&amp;gt; Int = &amp;lt;function1&amp;gt;\n\nscala&amp;gt; addOne\n&amp;lt;console&amp;gt;:9: error: missing arguments for method addOne;\nfollow this method with `_&amp;#39; if you want to treat it as a partially applied function\n              addOne\n              ^\n\nscala&amp;gt; res1\nres3: Int =&amp;gt; Int = &amp;lt;function1&amp;gt;\n&lt;/code&gt;&lt;/pre&gt;\n&lt;/div&gt;&lt;!-- SC_ON --&gt;", "selftext": "Hello, how would I get the value of the defined function `addOne` in the session below? simply typing `addOne` into the console did not return the value of the function, while typing `res1` returned the value of the anonymous function.\n\n    scala&gt; def addOne(m: Int): Int = m + 1\n    addOne: (m: Int)Int\n    \n    scala&gt; val three = addOne(2)\n    three: Int = 3\n    \n    scala&gt; (x: Int) =&gt; x + 1\n    res1: Int =&gt; Int = &lt;function1&gt;\n    \n    scala&gt; addOne\n    &lt;console&gt;:9: error: missing arguments for method addOne;\n    follow this method with `_' if you want to treat it as a partially applied function\n                  addOne\n                  ^\n\n    scala&gt; res1\n    res3: Int =&gt; Int = &lt;function1&gt;\n", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2k6iio", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "metaperl", "media": null, "score": 6, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": true, "name": "t3_2k6iio", "permalink": "/r/scala/comments/2k6iio/sbt_console_getting_value_of_defined_function/", "stickied": false, "created": 1414168974.0, "url": "http://www.reddit.com/r/scala/comments/2k6iio/sbt_console_getting_value_of_defined_function/", "author_flair_text": null, "title": "sbt console: getting value of defined function", "created_utc": 1414140174.0, "ups": 6, "num_comments": 6, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "self.scala", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": "&lt;!-- SC_OFF --&gt;&lt;div class=\"md\"&gt;&lt;p&gt;Hi!\nI just started a Spray project after having worked on Play! for a little.\nSpray is pretty amazing but also I think I&amp;#39;m having a tough time figuring out the proper way to use the Actor model (aside from just the Http Service routers).&lt;/p&gt;\n\n&lt;p&gt;I&amp;#39;d love if someone could peed around my initial project: &lt;a href=\"https://github.com/fzakaria/addressme\"&gt;https://github.com/fzakaria/addressme&lt;/a&gt;&lt;/p&gt;\n\n&lt;p&gt;and offer some free advice :)\nI&amp;#39;m still figuring out a good way to incorporate a DOA layer using Slick &lt;/p&gt;\n&lt;/div&gt;&lt;!-- SC_ON --&gt;", "selftext": "Hi!\nI just started a Spray project after having worked on Play! for a little.\nSpray is pretty amazing but also I think I'm having a tough time figuring out the proper way to use the Actor model (aside from just the Http Service routers).\n\nI'd love if someone could peed around my initial project: https://github.com/fzakaria/addressme\n\nand offer some free advice :)\nI'm still figuring out a good way to incorporate a DOA layer using Slick ", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2k4u13", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "Setheron", "media": null, "score": 6, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": true, "name": "t3_2k4u13", "permalink": "/r/scala/comments/2k4u13/looking_for_some_initial_feedback_on_spray_project/", "stickied": false, "created": 1414102530.0, "url": "http://www.reddit.com/r/scala/comments/2k4u13/looking_for_some_initial_feedback_on_spray_project/", "author_flair_text": null, "title": "Looking for some initial feedback on Spray project", "created_utc": 1414098930.0, "ups": 6, "num_comments": 18, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "loicdescotte.github.io", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": null, "selftext": "", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2k2kxz", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "loicd", "media": null, "score": 12, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": false, "name": "t3_2k2kxz", "permalink": "/r/scala/comments/2k2kxz/my_favorite_way_of_injecting_dependencies_in_scala/", "stickied": false, "created": 1414070683.0, "url": "http://loicdescotte.github.io/posts/scala-di/", "author_flair_text": null, "title": "My favorite way of injecting dependencies in Scala", "created_utc": 1414041883.0, "ups": 12, "num_comments": 16, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "self.scala", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": "&lt;!-- SC_OFF --&gt;&lt;div class=\"md\"&gt;&lt;p&gt;I was wondering if someone could help me understand exactly what this code is doing.... &lt;/p&gt;\n\n&lt;p&gt;def map(s: Set, f: Int =&amp;gt; Int): Set = (e: Int) =&amp;gt; exists(s, f(_) == e)&lt;/p&gt;\n\n&lt;p&gt;Especially the part of: f: Int =&amp;gt; Int. \nEvery time that I try to call that function and put an Int it says that I have  Type mismatch; found Int, Require Int =&amp;gt; Int.\nThanks for the help, I&amp;#39;m a noob at scala.&lt;/p&gt;\n&lt;/div&gt;&lt;!-- SC_ON --&gt;", "selftext": "I was wondering if someone could help me understand exactly what this code is doing.... \n\n\ndef map(s: Set, f: Int =&gt; Int): Set = (e: Int) =&gt; exists(s, f(_) == e)\n\nEspecially the part of: f: Int =&gt; Int. \nEvery time that I try to call that function and put an Int it says that I have  Type mismatch; found Int, Require Int =&gt; Int.\nThanks for the help, I'm a noob at scala.", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2k2nkv", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "edwrx02", "media": null, "score": 3, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": true, "name": "t3_2k2nkv", "permalink": "/r/scala/comments/2k2nkv/can_someone_please_help_me_understand_this_in/", "stickied": false, "created": 1414047726.0, "url": "http://www.reddit.com/r/scala/comments/2k2nkv/can_someone_please_help_me_understand_this_in/", "author_flair_text": null, "title": "Can someone please help me understand this in Scala.", "created_utc": 1414044126.0, "ups": 3, "num_comments": 7, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "scalar-conf.com", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": null, "selftext": "", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2jzvhp", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "wozmir", "media": null, "score": 20, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": false, "name": "t3_2jzvhp", "permalink": "/r/scala/comments/2jzvhp/scalar_2015_announced_free_conference_by_the/", "stickied": false, "created": 1414017822.0, "url": "http://scalar-conf.com/", "author_flair_text": null, "title": "Scalar 2015 announced (free conference by the Scala Times creators)", "created_utc": 1413989022.0, "ups": 20, "num_comments": 2, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "youtube.com", "banned_by": null, "media_embed": {"content": "&lt;iframe class=\"embedly-embed\" src=\"//cdn.embedly.com/widgets/media.html?src=http%3A%2F%2Fwww.youtube.com%2Fembed%2FlgyGFG6hBa0%3Ffeature%3Doembed&amp;url=http%3A%2F%2Fwww.youtube.com%2Fwatch%3Fv%3DlgyGFG6hBa0&amp;image=http%3A%2F%2Fi.ytimg.com%2Fvi%2FlgyGFG6hBa0%2Fhqdefault.jpg&amp;key=2aa3c4d5f3de4f5b9120b660ad850dc9&amp;type=text%2Fhtml&amp;schema=youtube\" width=\"600\" height=\"338\" scrolling=\"no\" frameborder=\"0\" allowfullscreen&gt;&lt;/iframe&gt;", "width": 600, "scrolling": false, "height": 338}, "subreddit": "scala", "selftext_html": null, "selftext": "", "likes": null, "user_reports": [], "secure_media": {"type": "youtube.com", "oembed": {"provider_url": "http://www.youtube.com/", "description": "ScalaCheck, the property-based testing library for Scala, is a powerful tool for automating test coverage. Out of the box, you can easily generate gobs of test data and automatically shrink failure cases down to specific causes. Who was ever satisfied with out of the box, though?!?", "title": "SBTB 2014, Kelsey Gilmore-Innis:: I Dream of Gen'ning: ScalaCheck Beyond the Basics", "url": "http://www.youtube.com/watch?v=lgyGFG6hBa0", "author_name": "FunctionalTV", "height": 338, "width": 600, "html": "&lt;iframe class=\"embedly-embed\" src=\"https://cdn.embedly.com/widgets/media.html?src=https%3A%2F%2Fwww.youtube.com%2Fembed%2FlgyGFG6hBa0%3Ffeature%3Doembed&amp;url=http%3A%2F%2Fwww.youtube.com%2Fwatch%3Fv%3DlgyGFG6hBa0&amp;image=http%3A%2F%2Fi.ytimg.com%2Fvi%2FlgyGFG6hBa0%2Fhqdefault.jpg&amp;key=2aa3c4d5f3de4f5b9120b660ad850dc9&amp;type=text%2Fhtml&amp;schema=youtube\" width=\"600\" height=\"338\" scrolling=\"no\" frameborder=\"0\" allowfullscreen&gt;&lt;/iframe&gt;", "thumbnail_width": 480, "version": "1.0", "provider_name": "YouTube", "thumbnail_url": "https://i.embed.ly/1/image?url=http%3A%2F%2Fi.ytimg.com%2Fvi%2FlgyGFG6hBa0%2Fhqdefault.jpg&amp;key=b1e305db91cf4aa5a86b732cc9fffceb", "type": "video", "thumbnail_height": 360, "author_url": "http://www.youtube.com/user/FunctionalTV"}}, "link_flair_text": null, "id": "2jx1k8", "gilded": 0, "secure_media_embed": {"content": "&lt;iframe class=\"embedly-embed\" src=\"https://cdn.embedly.com/widgets/media.html?src=https%3A%2F%2Fwww.youtube.com%2Fembed%2FlgyGFG6hBa0%3Ffeature%3Doembed&amp;url=http%3A%2F%2Fwww.youtube.com%2Fwatch%3Fv%3DlgyGFG6hBa0&amp;image=http%3A%2F%2Fi.ytimg.com%2Fvi%2FlgyGFG6hBa0%2Fhqdefault.jpg&amp;key=2aa3c4d5f3de4f5b9120b660ad850dc9&amp;type=text%2Fhtml&amp;schema=youtube\" width=\"600\" height=\"338\" scrolling=\"no\" frameborder=\"0\" allowfullscreen&gt;&lt;/iframe&gt;", "width": 600, "scrolling": false, "height": 338}, "clicked": false, "report_reasons": null, "author": "mark__", "media": {"type": "youtube.com", "oembed": {"provider_url": "http://www.youtube.com/", "description": "ScalaCheck, the property-based testing library for Scala, is a powerful tool for automating test coverage. Out of the box, you can easily generate gobs of test data and automatically shrink failure cases down to specific causes. Who was ever satisfied with out of the box, though?!?", "title": "SBTB 2014, Kelsey Gilmore-Innis:: I Dream of Gen'ning: ScalaCheck Beyond the Basics", "url": "http://www.youtube.com/watch?v=lgyGFG6hBa0", "author_name": "FunctionalTV", "height": 338, "width": 600, "html": "&lt;iframe class=\"embedly-embed\" src=\"//cdn.embedly.com/widgets/media.html?src=http%3A%2F%2Fwww.youtube.com%2Fembed%2FlgyGFG6hBa0%3Ffeature%3Doembed&amp;url=http%3A%2F%2Fwww.youtube.com%2Fwatch%3Fv%3DlgyGFG6hBa0&amp;image=http%3A%2F%2Fi.ytimg.com%2Fvi%2FlgyGFG6hBa0%2Fhqdefault.jpg&amp;key=2aa3c4d5f3de4f5b9120b660ad850dc9&amp;type=text%2Fhtml&amp;schema=youtube\" width=\"600\" height=\"338\" scrolling=\"no\" frameborder=\"0\" allowfullscreen&gt;&lt;/iframe&gt;", "thumbnail_width": 480, "version": "1.0", "provider_name": "YouTube", "thumbnail_url": "http://i.ytimg.com/vi/lgyGFG6hBa0/hqdefault.jpg", "type": "video", "thumbnail_height": 360, "author_url": "http://www.youtube.com/user/FunctionalTV"}}, "score": 12, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": false, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": false, "name": "t3_2jx1k8", "permalink": "/r/scala/comments/2jx1k8/scalacheck_beyond_the_basics_from_sbtb_2014_35_min/", "stickied": false, "created": 1413948567.0, "url": "https://www.youtube.com/watch?v=lgyGFG6hBa0", "author_flair_text": null, "title": "ScalaCheck Beyond the Basics (from SBTB 2014, 35 min)", "created_utc": 1413919767.0, "ups": 12, "num_comments": 0, "visited": false, "num_reports": null, "distinguished": null}}, {"kind": "t3", "data": {"domain": "self.scala", "banned_by": null, "media_embed": {}, "subreddit": "scala", "selftext_html": "&lt;!-- SC_OFF --&gt;&lt;div class=\"md\"&gt;&lt;p&gt;Hello &lt;a href=\"/r/scala\"&gt;/r/scala&lt;/a&gt;!&lt;/p&gt;\n\n&lt;p&gt;I am very happy to announce the first release of s_mach.concurrent &lt;a href=\"https://github.com/S-Mach/s_mach.concurrent\"&gt;https://github.com/S-Mach/s_mach.concurrent&lt;/a&gt;&lt;/p&gt;\n\n&lt;p&gt;s_mach.concurrent is an open-source Scala library that provides asynchronous serial and parallel execution flow control primitives for working with asynchronous tasks. An asynchronous task consists of two or more calls to function(s) that return a future result &lt;code&gt;A \u21d2 Future[B]&lt;/code&gt; instead of the result &lt;code&gt;A \u21d2 B&lt;/code&gt;. s_mach.concurrent also provides utility &amp;amp; convenience code for working with scala.concurrent.Future.&lt;/p&gt;\n\n&lt;ul&gt;\n&lt;li&gt;Adds concurrent flow control primitives async and async.par for performing fixed size heterogeneous (tuple) and variable size homogeneous (collection) asynchronous tasks. These primitives:\n\n&lt;ul&gt;\n&lt;li&gt;Allow enabling optional progress reporting, failure retry and/or throttle control for asynchronous tasks&lt;/li&gt;\n&lt;li&gt;Ensure proper sequencing of returned futures, e.g. given &lt;code&gt;f: Int \u21d2 Future[String]&lt;/code&gt;:\n\n&lt;ul&gt;\n&lt;li&gt;&lt;code&gt;List(1,2,3).async.map(f)&lt;/code&gt; returns &lt;code&gt;Future[List[String]]&lt;/code&gt;&lt;/li&gt;\n&lt;li&gt;&lt;code&gt;async.par.run(f(1),f(2),f(3))&lt;/code&gt; returns &lt;code&gt;Future[(String,String,String)]&lt;/code&gt;&lt;/li&gt;\n&lt;/ul&gt;&lt;/li&gt;\n&lt;li&gt;Ensures fail-immediate sequencing of future results&lt;/li&gt;\n&lt;li&gt;Ensures all exceptions generated during asynchronous task processing can be retrieved (Future.sequence returns only the first)&lt;/li&gt;\n&lt;li&gt;collection.async and collection.async.par support collection operations such as map, flatMap and foreach on asynchronous functions, i.e. &lt;code&gt;A \u21d2 Future[B]&lt;/code&gt;&lt;/li&gt;\n&lt;li&gt;async.par.run(future1, future2, \u2026) supports running fixed size heterogeneous asynchronous task (of up to 22 futures) in parallel&lt;/li&gt;\n&lt;/ul&gt;&lt;/li&gt;\n&lt;li&gt;Adds ScheduledExecutionContext, a Scala interface wrapper for java.util.concurrent.ScheduledExecutorService that provides for scheduling delayed and periodic tasks&lt;/li&gt;\n&lt;li&gt;Adds non-blocking concurrent control primitives such as Barrier, Latch, Lock and Semaphore&lt;/li&gt;\n&lt;li&gt;Provides convenience methods for writing more readable, concise and DRY concurrent code such as Future.get, Future.toTry and Future.fold&lt;/li&gt;\n&lt;/ul&gt;\n\n&lt;p&gt;I look forward to your feedback.&lt;/p&gt;\n\n&lt;p&gt;Thanks and may your day be awesome!&lt;/p&gt;\n&lt;/div&gt;&lt;!-- SC_ON --&gt;", "selftext": "Hello /r/scala!\n\nI am very happy to announce the first release of s_mach.concurrent https://github.com/S-Mach/s_mach.concurrent\n\ns_mach.concurrent is an open-source Scala library that provides asynchronous serial and parallel execution flow control primitives for working with asynchronous tasks. An asynchronous task consists of two or more calls to function(s) that return a future result `A \u21d2 Future[B]` instead of the result `A \u21d2 B`. s_mach.concurrent also provides utility &amp; convenience code for working with scala.concurrent.Future.\n\n* Adds concurrent flow control primitives async and async.par for performing fixed size heterogeneous (tuple) and variable size homogeneous (collection) asynchronous tasks. These primitives:\n  * Allow enabling optional progress reporting, failure retry and/or throttle control for asynchronous tasks\n  * Ensure proper sequencing of returned futures, e.g. given `f: Int \u21d2 Future[String]`:\n      * `List(1,2,3).async.map(f)` returns `Future[List[String]]`\n      * `async.par.run(f(1),f(2),f(3))` returns `Future[(String,String,String)]`\n  * Ensures fail-immediate sequencing of future results\n  * Ensures all exceptions generated during asynchronous task processing can be retrieved (Future.sequence returns only the first)\n  * collection.async and collection.async.par support collection operations such as map, flatMap and foreach on asynchronous functions, i.e. `A \u21d2 Future[B]`\n  * async.par.run(future1, future2, \u2026) supports running fixed size heterogeneous asynchronous task (of up to 22 futures) in parallel\n* Adds ScheduledExecutionContext, a Scala interface wrapper for java.util.concurrent.ScheduledExecutorService that provides for scheduling delayed and periodic tasks\n* Adds non-blocking concurrent control primitives such as Barrier, Latch, Lock and Semaphore\n* Provides convenience methods for writing more readable, concise and DRY concurrent code such as Future.get, Future.toTry and Future.fold\n\nI look forward to your feedback.\n\nThanks and may your day be awesome!", "likes": null, "user_reports": [], "secure_media": null, "link_flair_text": null, "id": "2jspg5", "gilded": 0, "secure_media_embed": {}, "clicked": false, "report_reasons": null, "author": "lancegatlin", "media": null, "score": 9, "approved_by": null, "over_18": false, "hidden": false, "thumbnail": "", "subreddit_id": "t5_2qh37", "edited": 1413829708.0, "link_flair_css_class": null, "author_flair_css_class": null, "downs": 0, "mod_reports": [], "saved": false, "is_self": true, "name": "t3_2jspg5", "permalink": "/r/scala/comments/2jspg5/ann_v10_release_of_s_machconcurrent_a_utility/", "stickied": false, "created": 1413826306.0, "url": "http://www.reddit.com/r/scala/comments/2jspg5/ann_v10_release_of_s_machconcurrent_a_utility/", "author_flair_text": null, "title": "ANN] v1.0 release of s_mach.concurrent a utility library for asynchronous tasks and scala.concurrent.Future", "created_utc": 1413822706.0, "ups": 9, "num_comments": 2, "visited": false, "num_reports": null, "distinguished": null}}], "after": "t3_2jspg5", "before": null}}""")
			println(result)
			result2 = jsonGenerator.dumps(result)
			println (result2)
			endTime = java.lang.System.currentTimeMillis()
			if (a > 2){
				runTime = endTime-startTime
				totalTime += runTime
			}
		}
		
		println(totalTime/1000 + " Seconds for tests")
  	}
}