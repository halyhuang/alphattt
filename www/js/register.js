var jsonrpc = imprt("jsonrpc");
var service = new jsonrpc.ServiceProxy("auth.yaws", ["register"]);


$(function()
{
    $("#register_human").click(function()
    {
		register("human");
    });
	
	$("#register_robot").click(function()
    {
		register("robot");
    });
	
	function register(type)
	{
		var userName = $("#username").val();
		var password = $("#pw").val();
        var passwordAgain = $("#pw_agin").val();

		if(userName == "" || password == "")
        {
            alert("用户名或密码不能为空！");
			return;
        }
		
		if(userName == password)
        {
            alert("用户名或密码不能相同！");
			return;
        }
		
        if (!(password == passwordAgain))
        {
            alert("两次密码输入不一致");
            return;
        }

		var result = service.register(userName, password, type);
        if (result.value)
        {
			alert("注册成功！");
            location.href = "login.html";
        }
		else
		{
			alert("注册失败！原因：" + result.reason);
		}
	}

});


	
