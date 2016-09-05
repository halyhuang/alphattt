var desks;

var jsonrpc = imprt("jsonrpc");

var service = new jsonrpc.ServiceProxy("hall.yaws", ["enter_room"]);

var auth_jsonrpc = imprt("jsonrpc");
var auth_service = new auth_jsonrpc.ServiceProxy("auth.yaws", ["init_session", "is_login"]);

function is_login()
{
	var r = auth_service.is_login();
	return r.value;
}

function init_session()
{
    try {
		auth_service.init_session();
     } catch(e) {
        alert(e);
     }	
}

window.onload = function() { 
 	init_session();
	init_hall();
};  

function init_hall()
{
	desks = document.querySelectorAll('.desk');
	for (var i=0; i < desks.length; i++)
	{ 
		desks[i].ID = i + 1;	
		desks[i].onclick = enter_room;
	}	
}

function enter_room()
{
    try {
			service.enter_room(this.ID);
			location.href = "alphattt.html";
     } catch(e) {
        alert(e);
     }	
}
