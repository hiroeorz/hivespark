var HS = HS || {};

/**
 * チーム情報の表示
 *
 * @namespace HS
 * @class TeamViewController
*/

HS.TeamViewController = function () { 'use strict'; this.init(); };

HS.TeamViewController.prototype = (function () {
    'use strict';

    /* private */

    /**
     * ユーザーの情報を取得してHTML上に配置します
     *
     * @method get_user_info
     */
    var get_user_info = function () {
	$.getJSON("/usr/show_myself",
		  function (json) {
		      $("div#profile img").attr("src", json.usr.icon_url);
		      $("span#my-project-count").text(json.usr.team_id_count);
		  });
    };
    
    /**
     * チームの情報を取得してHTML上に配置します
     *
     * @method get_team_info
     */
    var get_team_info = function (team_id) {
	$.getJSON("/team/info?team_id=" + team_id,
		  function (json) {
		      var team = json.team;
		      $("#project-name").text(team.name);
		      $("#project-description").text(team.description);
		      $("img#team-image").attr("src", team.icon_url);
		      $("div#member-area ul.members").empty();
		      $("div#team-status span").text(team.status_description);

		      for (var i = 0; i < team.members.length; i++) {
			  var usr = team.members[i];
			  var usr_icon_view = crate_usr_icon_view(usr);
			  $("div#member-area ul.members").append(usr_icon_view);
		      }

		      $("ul#add-new-team-menu").empty();

		      if (json.is_member == true) {
			  set_team_action(team.id, "このチームから抜ける",
					function() {delete_usr_from_team(team.id)});
		      } else {
			  set_team_action(team.id, "このチームに参加する", 
					function() {add_usr_to_team(team.id)});
		      }

		      if (json.is_owner == true) {
			  $("a#team-destroy").bind("click", function() {
			      destroy(team_id);
			  });
		      } else {
			  $("a#team-destroy").hide();
		      }
		  })
    };

    /**
     * 「チームへ参加」または「チームから抜ける」アクションをリンクに追加します
     *
     * @method set_team_action
     */
    var set_team_action = function (teamId, text, fun) {
	var add_team_a = document.createElement("a");
	add_team_a.src = "javascript:void(0)";
	add_team_a.onclick = fun;

	add_team_a.className = "thoughtbot";

	var add_team_text = document.createTextNode(text);
	add_team_a.appendChild(add_team_text);
	
	var add_team_li = document.createElement("li");
	add_team_li.appendChild(add_team_a);
	$("ul#add-new-team-menu").append(add_team_li);
    };

    /**
     * ユーザーをチームに追加します
     *
     * @method add_usr_to_team
     */
    var add_usr_to_team = function(teamId) {
	$.getJSON("/team/add_usr",  {team_id: teamId},
		  function (json) {
		      if (json.result == true) {
			  get_team_info(teamId);
		      } else {
			  alert("エラーが発生しました");
		      }
		  });	
    };

    /**
     * ユーザーをチームから外します
     *
     * @method delete_usr_from_team
     */
    var delete_usr_from_team = function(teamId) {
	$.getJSON("/team/delete_usr", {team_id: teamId},
		  function (json) {
		      if (json.result == true) {
			  get_team_info(teamId);
		      } else {
			  alert("エラーが発生しました");
		      }
		  });	
    };

    /**
     * 各工程のチケットを非同期に取得します
     *
     * @method get_articles
     */
    var get_articles = function (team_id) {
	$.getJSON("/article/teams_list?team_id=" + team_id + "&status=0",
		  function (articles) {
		      for (var i = 0; i < articles.length; i++) {
			  var article_view = create_issue_view(articles[i]);
			  $("ul#issues").append(article_view);
		      }
		  });
	$.getJSON("/article/teams_list?team_id=" + team_id + "&status=1",
		  function (articles) {
		      for (var i = 0; i < articles.length; i++) {
			  var article_view = create_article_view(articles[i]);
			  $("ul#stop-articles").append(article_view);
		      }
		  });
	$.getJSON("/article/teams_list?team_id=" + team_id + "&status=2",
		  function (articles) {
		      for (var i = 0; i < articles.length; i++) {
			  var article_view = create_article_view(articles[i]);
			  $("ul#active-articles").append(article_view);
		      }
		  });
	$.getJSON("/article/teams_list?team_id=" + team_id + "&status=3",
		  function (articles) {
		      for (var i = 0; i < articles.length; i++) {
			  var article_view = create_article_view(articles[i]);
			  $("ul#testing-articles").append(article_view);
		      }
		  });
	$.getJSON("/article/teams_list?team_id=" + team_id + "&status=4",
		  function (articles) {
		      for (var i = 0; i < articles.length; i++) {
			  var article_view = create_article_view(articles[i]);
			  $("ul#closedbeta-articles").append(article_view);
		      }
		  });
	$.getJSON("/article/teams_list?team_id=" + team_id + "&status=5",
		  function (articles) {
		      for (var i = 0; i < articles.length; i++) {
			  var article_view = create_article_view(articles[i]);
			  $("ul#released-articles").append(article_view);
		      }
		  });

    };

    /**
     * 提案チケットを実行チケットとして登録します
     *
     * @method set_ticket_from_issue
     */
    var set_ticket_from_issue = function(event) {
	var article = event.data.article;
	$.post("/article/status_increment", {article_id: article.id},
	       function(data) {
		   var json = JSON.parse(data);

		   if (true == json.result) {
		       $("#" + article_view_id(article)).remove();
		       var article_view = create_article_view(json.article);
		       $("ul#stop-articles").prepend(article_view);
		   } else {
		       alert("ステータスの更新に失敗しました")		       
		   }		   
	       })
    };

    /**
     * 各工程の要素IDを生成して返します
     *
     * @method article_status_selector
     */
    var article_status_selector = function(article) {
	var selector = "ul#stop-articles";
	
	switch(article.status) {
	case 5: selector = "ul#released-articles"  ; break;
	case 4: selector = "ul#closedbeta-articles"; break;
	case 3: selector = "ul#testing-articles"   ; break;
	case 2: selector = "ul#active-articles"    ; break;
	case 1: selector = "ul#stop-articles"      ; break;
	case 0: selector = "ul#issues"             ; break;
	}

	return selector;
    };

    /**
     * チケットのステップを次の工程に移します
     *
     * @method incr_article
     */
    var incr_article = function(event) {
	var article = event.data.article;
	$.post("/article/status_increment", {article_id: article.id},
	       function(data) {
		   var json = JSON.parse(data);

		   if (true == json.result) {
		       $("#" + article_view_id(article)).remove();
		       var selector = article_status_selector(json.article);
		       var article_view = create_article_view(json.article);
		       $(selector).prepend(article_view);
		   } else {
		       alert("ステータスの更新に失敗しました")
		   }
	       })
    };

    /**
     * チケットのステップを一つ前の工程に差し戻します
     *
     * @method decr_article
     */
    var decr_article = function(event) {
	var article = event.data.article;
	$.post("/article/status_decrement", {article_id: article.id},
	       function(data) {
		   var json = JSON.parse(data);

		   if (true == json.result) {
		       $("#" + article_view_id(article)).remove();
		       var selector = article_status_selector(json.article);
		       var article_view = null;
		       
		       if (json.article.status > 0) {
			   article_view = create_article_view(json.article);
		       } else {
			   article_view = create_issue_view(json.article);
		       }
		       
		       $(selector).append(article_view);
		   } else {
		       alert("ステータスの更新に失敗しました")
		   }

	       })
    };

    /**
     * ステップチケットの要素IDを生成して返します
     *
     * @method article_view_id
     */
    var article_view_id = function(article) {
	return ("article_" + article.id);
    };

    var full_progress_width = 300;

    /**
     * 提案チケットのビューを生成します
     *
     * @method create_issue_view
     */
    var create_issue_view = function(article) {
	var article_li = document.createElement("li");
	article_li.id = article_view_id(article);
	
	var incr_button = document.createElement("button")
	incr_button.className = "thoughtbot set_step";
	incr_button.style.cssText = "width:70px;";
	var button_text = document.createTextNode("採用");
	incr_button.appendChild(button_text);
	$(incr_button).bind("click", {article:article}, set_ticket_from_issue);

	var usr = article.usr;
	var usr_img = document.createElement("img");
	usr_img.src = usr.icon_url;
	usr_img.alt = usr.name;

	var title_h3 = document.createElement("h3");
	var title = document.createTextNode(article.title);
	title_h3.appendChild(title);

	var text_p = document.createElement("p");
	var text = document.createTextNode(article.text);
	text_p.appendChild(text);

	var step_graph_val_div = document.createElement("div");
	step_graph_val_div.className = "step_graph_value";
	if (article.progress > 70) {
	    step_graph_val_div.className = "step_graph_value executing";
	}

	var clear_div = document.createElement("div");
	clear_div.style.cssText = "clear:both";

	article_li.appendChild(incr_button);
	article_li.appendChild(usr_img);
	article_li.appendChild(title_h3);
	article_li.appendChild(text_p);
	article_li.appendChild(clear_div);

	return article_li;
    };

    /**
     * チケットの内容を更新します
     *
     * @method update_article
     */
    var update_article = function(event) {
	var edit_space_title = event.data.edit_space_title;
	var edit_space_text = event.data.edit_space_text;
	var edit_space_progress = event.data.edit_space_progress;
	var article = event.data.article;

	var title = $(edit_space_title).val();
	var text = $(edit_space_text).val();
	var progress = $(edit_space_progress).val();

	$.post("/article/update", 
	       {article_id:article.id, 
		title:title, text:text, progress:progress},
	       function(data) {
		   var json = JSON.parse(data);
		   update_progress_callback(json, article);
	       })
    };

    /**
     * チケットの進捗値を更新します
     *
     * @method update_article_progress
     */
    var update_article_progress = function(event) {
	var article = event.data.article;
	var select = event.data.select;

	var title = article.title;
	var text = article.text;
	var progress = $(select).val();

	$.post("/article/update", 
	       {article_id:article.id, 
		title:title, text:text, progress:progress},
	       function(data) {
		   var json = JSON.parse(data);
		   update_progress_callback(json, article);
	       });
    };

    /**
     * チケットを更新した時のコールバッック関数
     *
     * @method update_progress_callback
     */
    var update_progress_callback = function(json, original_article) {	
	if (true == json.result) {
	    $("#" + article_view_id(original_article)).remove();
	    var article_view = create_article_view(json.article);
	    var selector = article_status_selector(json.article);
	    $(selector).append(article_view);
	} else {
	    alert("ステータスの更新に失敗しました")		       
	}
    };
        
    /**
     * チケットの編集用ビューを生成して返す
     *
     * @method create_article_edit_space
     */
    var create_article_edit_space = function(article) {
	var edit_space = document.createElement("div");
	edit_space.className = "highslide-maincontent";

	var edit_space_title_label = document.createElement("label");
	var edit_space_title_label_text = document.createTextNode("タイトル");
	edit_space_title_label.appendChild(edit_space_title_label_text);
	var edit_space_title = document.createElement("input");
	edit_space_title.type = "text";
	edit_space_title.value = article.title;
	edit_space_title.style.cssText = "width:300px;";
	edit_space.appendChild(edit_space_title_label);
	edit_space.appendChild(edit_space_title);

	var edit_space_br = document.createElement("br");
	edit_space.appendChild(edit_space_br);

	var edit_space_text = document.createElement("textarea");
	var edit_space_text_label = document.createElement("label");
	var edit_space_text_label_text = document.createTextNode("状況　　");
	edit_space_text_label.appendChild(edit_space_text_label_text);
	var edit_text = document.createTextNode(article.text);
	edit_space_text.appendChild(edit_text);
	edit_space_text.style.cssText = "width:300px; height:200px;";
	edit_space.appendChild(edit_space_text_label);
	edit_space.appendChild(edit_space_text);

	edit_space_br = document.createElement("br");
	edit_space.appendChild(edit_space_br);

	var edit_space_progress_label = document.createElement("label");
	var edit_space_progress_label_text = document.createTextNode("進捗　　");
	edit_space_progress_label.appendChild(edit_space_progress_label_text);
	var edit_space_progress = document.createElement("input");
	edit_space_progress.type = "text";
	edit_space_progress.value = article.progress;
	edit_space_progress.style.cssText = "width:100px;";
	edit_space.appendChild(edit_space_progress_label);
	edit_space.appendChild(edit_space_progress);

	edit_space_br = document.createElement("br");
	edit_space.appendChild(edit_space_br);

	var save_button = document.createElement("input");
	save_button.type = "submit";
	save_button.value = "保存";
	$(save_button).bind("click", 
			    {edit_space_title:edit_space_title,
			     edit_space_text:edit_space_text,
			     edit_space_progress:edit_space_progress, 
			     article: article},
			    update_article);

	edit_space.appendChild(save_button);
	
	return edit_space;
    };

    /**
     * チケットのビューを生成して返す
     *
     * @method create_article_view
     */
    var create_article_view = function(article) {
	var article_li = document.createElement("li");
	article_li.id = article_view_id(article);

	var decr_button = document.createElement("button")
	decr_button.className = "thoughtbot-gray set_step_negative";
	var button_text = document.createTextNode("↑戻す");
	decr_button.appendChild(button_text);
	$(decr_button).bind("click", {article:article}, decr_article);

	var incr_button = document.createElement("button")
	incr_button.className = "thoughtbot set_step";
	var button_text = document.createTextNode("↓担当する");
	incr_button.appendChild(button_text);
	$(incr_button).bind("click", {article:article}, incr_article);
	
	var clear_div = document.createElement("div");
	clear_div.style.cssText = "clear:both;";

	var usr = article.usr;
	var usr_img = document.createElement("img");
	usr_img.src = usr.icon_url;
	usr_img.alt = usr.name;

	var title_h3 = document.createElement("h3");
	var title = document.createTextNode(article.title);
	title_h3.appendChild(title);
	var title_a = document.createElement("a");
	title_a.href = "javascript:void(0)";

	$(title_a).bind("click", function() {
	    return hs.htmlExpand(this);
	});
	title_a.appendChild(title_h3);

	var edit_space = create_article_edit_space(article);

	var text_p = document.createElement("p");
	var text = document.createTextNode(article.text);
	text_p.appendChild(text);

	var step_h4 = document.createElement("h4");
	var progress_str = "進捗: 0%";

	if (article.progress != null) {
	    progress_str = "進捗: " + article.progress + "%";
	}
	var step_text = document.createTextNode(progress_str);
	step_h4.appendChild(step_text);

	var step_graph_div = document.createElement("div");
	step_graph_div.className = "step_graph";

	var step_graph_val_div = document.createElement("div");
	step_graph_val_div.className = "step_graph_value";
	if (article.progress >= 100) {
	    step_graph_val_div.className = "step_graph_value executing";
	}
	var progress_width = full_progress_width * (article.progress / 100);
	step_graph_val_div.style.cssText = "width: " + progress_width + "px";
	step_graph_div.appendChild(step_graph_val_div);
	
	var clear_div = document.createElement("div");
	clear_div.style.cssText = "clear:both";

	var progress_select = document.createElement("select");

	for (var i = 100; i >= 0; i -= 10) {
	    var option = document.createElement("option");
	    option.value = i;
	    var option_text = document.createTextNode(i);
	    option.appendChild(option_text);
	    
	    if (article.progress == i) {
		option.selected = "selected";
	    }

	    progress_select.appendChild(option);
	}

	$(progress_select).bind("change", 
				{article:article, select:progress_select},
				update_article_progress);

	article_li.appendChild(decr_button);

	if (article.status < 2 || 
	    (article.progress >= 100 && article.status < 5)) {
	    article_li.appendChild(incr_button);
	}

	article_li.appendChild(clear_div);
	article_li.appendChild(usr_img);
	article_li.appendChild(title_a);
	article_li.appendChild(edit_space);
	article_li.appendChild(text_p);
	article_li.appendChild(clear_div);

	if (article.status > 1 && article.status < 5) {
	    article_li.appendChild(step_h4);
	    article_li.appendChild(step_graph_div);
	    article_li.appendChild(progress_select);
	}
	return article_li;
    };

    var crate_usr_icon_view = function(usr) {
	var usr_li = document.createElement("li");
	var usr_img = document.createElement("img");
	usr_img.src = usr.icon_url;
	usr_img.alt = usr.name;
	usr_li.appendChild(usr_img);
	return usr_li;
    };

    /**
     * サブメニューのリンク設定を行う
     *
     * @method set_team_menu
     */
    var set_team_menu = function(team_id) {
	$("a#team-edit").attr("href", "/team/edit?team_id=" + team_id);
	$("a#team-upload-icon").attr("href", 
				     "/team/upload_icon?team_id=" + team_id);
    };

    /**
     * サブメニューの削除を行う
     *
     * @method destroy
     */
    var destroy = function(team_id) {
	if (confirm("本当にこのチームを削除しますか？")) {
	    $.post("/team/delete", {team_id:team_id},
		   function(data) {
		       var json = JSON.parse(data);
		       if (json.result == true) {
			   alert("削除しました");
			   location.href = "/team/index"
		       }
		   });
	}
    }

    /**
     * 新しいチケットを発行する
     *
     * @method add_new_ticket
     */
    var add_new_ticket = function(event) {
	var team_id = event.data.team_id;
	var title = $("#new-ticket-title").val();
	var text = $("#new-ticket-text").val();
	$.post("/article/create", 
	       {team_id:team_id, title:title, text:text},
	       function(data) {
		   var json = JSON.parse(data);
		   if (json.result == true) {
		       var article_view = create_issue_view(json.article);
		       $("ul#issues").append(article_view);
		   } else {
		       alert("登録に失敗しました¥n(reason:" + json.reason + ")");
		   }
	       })
    };

    /**
     * 新しいメッセージを投稿する
     *
     * @method add_new_message
     */
    var add_new_message = function(event) {
	var team_id = event.data.team_id;
	var text = $("#new-message-text").val();
	$.post("/team/send_message", 
	       {team_id:team_id, text:text},
	       function(data) {
		   var json = JSON.parse(data);
		   if (json.result != true) { alert("送信に失敗しました"); }
		   get_new_messages(team_id);
	       })
    };

    /**
     * 各オブジェクトのイベントリスナを登録します
     *
     * @method set_default_event_lister
     */
    var set_default_event_lister = function(params) {
	$("#new-ticket-button").bind("click", {team_id: params.team_id}, 
				     add_new_ticket);

	$("#new-message-button").bind("click", {team_id: params.team_id}, 
				      add_new_message);

	$("#message-refresh_button").bind("click", function() { 
	    get_new_messages(params.team_id);
	});
    };

    var last_message_id = 0;
    var new_message_color = "#fff0f5"

    /**
     * 表示しているチームのメッセージを取得して表示します
     *
     * @method get_messages
     */
    var set_messages_to_timeline = function(messages, color) {
	if (color == undefined) { color = "#fff"; }

	for (var i in messages) {
	    var message = messages[i];
	    var message_li = create_message_view(message);
	    $("ul#timeline").prepend(message_li);
	    $(message_li).css("background-color", color);
	    last_message_id = message.id;
	}
    };

    var get_messages = function(team_id) {
	$("ul#timeline").empty();
	$.getJSON("/team/get_messages?team_id=" + team_id,
		  function(messages) {
		      set_messages_to_timeline(messages);
		  });
    };

    var get_new_messages = function(team_id, fun) {
	$.getJSON("/team/get_new_messages?team_id=" + team_id + 
		  "&since_id=" + last_message_id,
		  function(messages) {
		      if (messages.length > 0) {
			  $("ul#timeline li").css("background-color", "#fff");
		      }

		      $("ul#timeline li").css("background-color", "#fff");
		      set_messages_to_timeline(messages, new_message_color);
		  });
    };

    var start_get_message_interval = function(team_id, interval) {
	$.getJSON("/team/get_new_messages?team_id=" + team_id + 
		  "&since_id=" + last_message_id,
		  function(messages) {
		      if (messages.length > 0) {
			  $("ul#timeline li").css("background-color", "#fff");
			  set_messages_to_timeline(messages, new_message_color);
		      }
		      setTimeout(function() {
			  start_get_message_interval(team_id, interval);
		      }, interval);
		  });
    };

    /**
     * メッセージ要素のビューを作って返す
     *
     * @method create_message_li
     */
    var create_message_view = function(message) {
	var li = document.createElement("li");

	var img = document.createElement("img");
	img.src = message.usr.icon_url;
	img.alt = message.usr.name;

	var p = document.createElement("p");
	var text = document.createTextNode(message.text);
	p.appendChild(text);

	var clear_div1 = document.createElement("div");
	clear_div1.style.cssText = "clear:both;";

	var name_h3 = document.createElement("h3");
	var name_text = document.createTextNode(message.usr.name);
	name_h3.appendChild(name_text);

	var time_p = document.createElement("p");
	time_p.className = "tweet_time";
	var time_text = document.createTextNode(message.created_at);
	time_p.appendChild(time_text);

	var reply_a = document.createElement("a");
	reply_a.href = "#";

	var clear_div2 = document.createElement("div");
	clear_div2.style.cssText = "clear:both;";
	
	li.appendChild(img);
	li.appendChild(p);
	li.appendChild(clear_div1);
	li.appendChild(name_h3);
	li.appendChild(time_p);
	li.appendChild(reply_a);
	li.appendChild(clear_div2);

	return li;
    };

    /* 新規メッセージを確認するインターバル */
    //var check_new_message_interval = 1000 * 3;	
    var check_new_message_interval = 60000 * 1;	

    /* public */
    return {
	/**
	 * 初期化
	 *
	 * @method init
	 */
	init: function () {
	},

	/**
	 * 画面表示直後に呼ばれる
	 *
	 * @method view_did_load
	 */
	view_did_load: function () {
	    var self = this;
	    var params = new HS.Util().qs_vals();
	    set_default_event_lister(params);

	    get_user_info();
	    set_team_menu(params.team_id)
	    get_team_info(params.team_id);
	    get_articles(params.team_id);
	    get_messages(params.team_id);

	    setTimeout(function() { 
		start_get_message_interval(params.team_id, 
					   check_new_message_interval);
	    }, check_new_message_interval);
	}

    };
}());

