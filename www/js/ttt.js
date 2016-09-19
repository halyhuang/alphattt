

var jsonrpc = imprt("jsonrpc");
var service = new jsonrpc.ServiceProxy("alphattt.yaws", ["poll_get_move", "poll_display", "start_game", "start_robot", "start_observe", "get_move", "get_legal_moves", "set_move"]);
var hall_service = new jsonrpc.ServiceProxy("hall.yaws", ["get_room", "leave_room"]);
var auth_service = new jsonrpc.ServiceProxy("auth.yaws", ["is_login"]);
	
var grids;
var poll_timerID = 0;
var is_poll_get_move = false;
var is_poll_display = false;

var players = new Array();
players[0] = {player:'0', color:"white", innerHTML:""};
players[1] = {player:'1', color:"#00FFFF", innerHTML:"X"};
players[2] = {player:'2', color:"#53FF53", innerHTML:"O"};
var player = 1;

var legal_moves = new Array();

function is_login()
{
	var r = auth_service.is_login();
	return r.value;
}

function check_login()
{
	if (!is_login())
	{		
		location.href = "login.html";
	}
}

function check_room()
{
	var result = hall_service.get_room();
	if (result.room_id == 0)
	{
		alert("roomID invalid,please select a room");
		location.href = "hall.html";
	}
}

window.onload = function() {  
	check_login();
	check_room();
	init_botton();	
	init_board();		
	init_poll();
};  

window.onbeforeunload = function(event) 
{ 
	var	is_leave = confirm("是否离开房间");
	if (is_leave)
	{
		hall_service.leave_room();
	}
	return is_leave;
}

function init_poll()
{
	poll_timerID = setInterval(poll, 300);	
}


function poll()
{		
	poll_display();
	poll_get_move();
}


function poll_get_move()
{
    try {			
			if (is_poll_get_move)
			{
				var result = service.poll_get_move();
				if (result.is_get_move)
				{
					player = result.player;
					legal_moves = result.legal_moves;
				}	
			}				
     } catch(e) {
        alert(e);
     }	
	
}

function poll_display()
{
    try {			
			if (is_poll_display)
			{
				var result = service.poll_display();
				if (result.is_update_display)
				{
					if (result.moves.length == 0)
					{
						init_board();
					}		
					else
					{
						update_display(result.moves[0].player, result.moves[0].move);	
					}
				}	
				if (result.infos.length > 0)
				{
					for (var i=0; i < result.infos.length; i++)
					{ 
						var player = result.infos[i].player;
						info(players[player].player, result.infos[i].info);
					}
				}				
				set_legal_move(player);		
			}
     } catch(e) {
        alert(e);
     }	
	
}

function grid_pos(move)
{
	return ((move.R * 3 + move.r) * 9 + (move.C * 3 + move.c));
}

function update_display(player, move)
{
	set_backgroud_blank();
	var index = grid_pos(move);
	grids[index].state = player;
	grids[index].innerHTML = players[player].innerHTML;
	grids[index].style.background = players[player].color;
	info(players[player].player, "move(" + move.R + "," + move.C + "," + move.r + "," + move.c + ")");
}

function opponent(id)
{
	return 3 - id;
}

function init_botton()
{
    var bn_start = document.getElementById('start_game');  
	bn_start.onclick = start_game; 
    var bn_robot = document.getElementById('start_robot');  
	bn_robot.onclick = start_robot; 	
    var bn_observe = document.getElementById('start_observe');  
	bn_observe.onclick = start_observe; 	
    var bn_hall = document.getElementById('start_hall');  
	bn_hall.onclick = start_hall; 	
	
}  

function init_board()
{
	is_get_move = false;
	grids = document.querySelectorAll('.grid');
	for (var i=0; i < grids.length; i++)
	{ 
		grids[i].R = Math.floor((Math.floor(i / 9)) / 3);
		grids[i].C = Math.floor((i % 9) / 3);
		grids[i].r = (Math.floor(i / 9)) % 3;
		grids[i].c = i % 3;		
		grids[i].onclick = click_move;
	    grids[i].onmouseenter = enter_grid;
		grids[i].onmouseleave = leave_grid;	
		grids[i].style.background = players[0].color;	
		grids[i].innerHTML = players[0].innerHTML;
		grids[i].state = 0;
		grids[i].is_legal = false;		
	}
}

function set_backgroud_blank()
{
	for (var i=0; i < grids.length; i++)
	{ 
		grids[i].style.background = players[0].color;	
	}		
}

function set_backgroud_opponent(enter_grid, is_show)
{
	for (var i=0; i<grids.length; i++)
	{ 
		if (grids[i].R == enter_grid.r && grids[i].C == enter_grid.c && grids[i].state == 0)
		{
			if (is_show)
			{
				grids[i].style.background = players[opponent(player)].color;			
			}
			else
			{
				grids[i].style.background = players[0].color;								
			}
		}
	}	
}

function info(player, msg)
{
    var chatNewThread = document.createElement('li'),
    	chatNewMessage = document.createTextNode(msg);
		
	var att = document.createAttribute('player');
		att.value = player;
    // Add message to chat thread and scroll to bottom
    chatNewThread.appendChild(chatNewMessage);
	chatNewThread.setAttributeNode(att);
	var	chatThread = document.getElementById('chat-thread-result');
    chatThread.appendChild(chatNewThread);
    chatThread.scrollTop = chatThread.scrollHeight;	
}

function start_game()
{
	service.start_game();
	is_poll_get_move = true;
	is_poll_display = true;
}  

function start_robot()
{	
	service.start_robot();	
	is_poll_get_move = true;
	is_poll_display = true;
}

function start_observe()
{
	var result = service.start_observe();	
	init_observe(result.moves);
	is_poll_display = true;	
}

function init_observe(player_moves)
{
	for (var i = 0; i < player_moves.length; i++)
	{
		update_display(player_moves[i].player, player_moves[i].move);		
	}
}

function start_hall()
{
    try {
			location.href = "hall.html";
     } catch(e) {
        alert(e);
     }	
}

function set_grid_inlegal()
{
	for (var i=0; i < grids.length; i++)
	{ 
		grids[i].is_legal = false;	
	}	
}

function set_legal_move()
{
	set_grid_inlegal();
	set_backgroud_legal();
}

function set_backgroud_legal()
{
	for (var i=0; i<legal_moves.length; i++)
	{ 
		var index = grid_pos(legal_moves[i]);
		grids[index].style.background = players[player].color;
		grids[index].is_legal = true;
	}		
}

function click_move()
{
	if (this.is_legal)
	{
		set_grid_inlegal();
		set_backgroud_blank();
		legal_moves = new Array();
		service.set_move(this.R, this.C, this.r, this.c);	
	}
}

function enter_grid()
{
	if (this.is_legal && this.state == 0)
	{
		set_backgroud_opponent(this, true);
		set_backgroud_legal();		
	}
}

function leave_grid()
{
	if (this.is_legal && this.state == 0)
	{	
		set_backgroud_opponent(this, false);
		set_backgroud_legal();	
	}
}