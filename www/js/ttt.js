

var jsonrpc = imprt("jsonrpc");
var service = new jsonrpc.ServiceProxy("alphattt.yaws", ["start_game", "start_robot", "get_state", "set_move"]);
var grids;
var timerID;
var player_background_color = '#00FFFF';
var opponent_move_color = '#53FF53';
var background_color = 'white';
var legal_moves;
var	is_playing = false;

var auth_jsonrpc = imprt("jsonrpc");
var auth_service = new auth_jsonrpc.ServiceProxy("auth.yaws", ["is_login"]);

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

window.onload = function() {  
	check_login();
	init_botton();	
};  
window.onbeforeunload = function(event) 
{ 
	var is_leave = true;
	if (is_playing)
	{
		is_leave = confirm("确定离开此页面吗？");
	}
	return is_leave;
}

function poll()
{
    try {
			var state = service.get_state();
			if (state.is_update_move)
			{
				legal_moves = state.legal_moves;			
				set_legal_move();			
				update_move(state.move);
			}	
     } catch(e) {
        alert(e);
     }	
	
}

function init_botton()
{
    var bn_start = document.getElementById('start_game');  
	bn_start.onclick = start_game; 
    var bn_robot = document.getElementById('start_robot');  
	bn_robot.onclick = start_robot; 	
    var bn_witness = document.getElementById('start_witness');  
	bn_witness.onclick = start_witness; 	
    var bn_hall = document.getElementById('start_hall');  
	bn_hall.onclick = start_hall; 	
	
}  

function init_board()
{
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
		grids[i].style.background = background_color;	
		grids[i].innerHTML = "";
		grids[i].state = 0;
		grids[i].is_legal = false;		
	}
}

function grid_pos(move)
{
	return ((move.R * 3 + move.r) * 9 + (move.C * 3 + move.c));
}

function update_move(move)
{
	if (move != "")
	{
		var index = grid_pos(move);
		info("move(" + move.R + "," + move.C + "," + move.r + "," + move.c + ")");
		grids[index].state = 2;
		grids[index].innerHTML = "O";
		grids[index].style.background = opponent_move_color;
	}
}

function set_backgroud_blank()
{
	for (var i=0; i < grids.length; i++)
	{ 
		grids[i].style.background = background_color;	
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
				grids[i].style.background = opponent_move_color;			
			}
			else
			{
				grids[i].style.background = background_color;								
			}
		}
	}	
}

function info(msg)
{
	document.getElementById('result').innerHTML +=
       "<li>" + msg + "</li>";
}

function set_timer()
{
	if( timerID == undefined)
	{	
		timerID = setInterval(poll, 1000);
	}
}

function start_game()
{
	is_playing = true;
	init_board();
	var result = service.start_game();
	info("start!");	
	set_timer();
}  

function start_robot()
{
	var result = service.start_robot();	
	info("robot start!");	
	set_timer();	
}

function start_witness()
{
	alert("not finish");
}

function start_hall()
{
	location.href = "hall.html";
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
	set_backgroud_blank();
	set_backgroud_legal();
}

function set_backgroud_legal()
{
	for (var i=0; i<legal_moves.length; i++)
	{ 
		var index = grid_pos(legal_moves[i]);
		grids[index].style.background = player_background_color;
		grids[index].is_legal = true;
	}		
}

function click_move()
{
	if (this.is_legal)
	{
		set_grid_inlegal();
		set_backgroud_blank();		
		service.set_move(this.R, this.C, this.r, this.c);	
		this.state = 1;
		this.innerHTML = "X";	
		this.style.background = player_background_color;
		info("move(" + this.R + "," + this.C + "," + this.r + "," + this.c + ")");
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