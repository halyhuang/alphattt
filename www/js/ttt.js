

var jsonrpc = imprt("jsonrpc");
var service = new jsonrpc.ServiceProxy("alphattt.yaws", ["play_vs_robot", "play_vs_human", "get_state", "set_move"]);
var grids;
var timerID = 0;


var hall_jsonrpc = imprt("jsonrpc");
var auth_service = new hall_jsonrpc.ServiceProxy("auth.yaws", ["create_session", "is_login"]);

function init_session()
{
    try 
	{
		auth_service.create_session();
	} 
	catch(e) 
	{
		alert(e);
	}	
}

function is_login()
{
	var r = auth_service.is_login();
	return r.value;
}

window.onload = function() {  
	init_session();
	init_botton();	
};  

function poll()
{
    try {
			var state = service.get_state();
			if (state.is_update_move)
			{
				set_legal_move(state.legal_moves);			
				update_move(state.move);
			}	
     } catch(e) {
        alert(e);
     }	
	
}

function init_botton()
{
    var bn_human = document.getElementById('play_human');  
	bn_human.onclick = play_vs_human; 
    var bn_robot = document.getElementById('play_robot');  
	bn_robot.onclick = play_vs_robot; 	
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
		grids[i].style.background = 'white';	
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
		grids[index].style.background = '#53FF53';
	}
}

function set_legal_move(legal_moves)
{
	for (var i=0; i < grids.length; i++)
	{ 
		grids[i].style.background = 'white';	
		grids[i].is_legal = false;	
	}	
	for (var i=0; i<legal_moves.length; i++)
	{ 
		var index = grid_pos(legal_moves[i]);
		grids[index].style.background = '#00FFFF';
		grids[index].is_legal = true;
	}	
}

function info(msg)
{
	document.getElementById('result').innerHTML +=
       "<li>" + msg + "</li>";
}

function play_vs_human()
{
	init_board();
	var result = service.play_vs_human();
	info("play vs human start!!!");	
	timerID = setInterval(poll, 1000);	
}  

function play_vs_robot()
{
	init_board();	
	var result = service.play_vs_robot();	
	info("play vs robot start!!!");	
	timerID = setInterval(poll, 1000);		
}

function click_move()
{
	if (this.is_legal)
	{
		service.set_move(this.R, this.C, this.r, this.c);	
		this.state = 1;
		this.innerHTML = "X";	
		info("move(" + this.R + "," + this.C + "," + this.r + "," + this.c + ")");
	}
}