var serviceURL = "alphattt.yaws";
var methods = [ "register"];

var jsonrpc = imprt("jsonrpc");
var service = new jsonrpc.ServiceProxy(serviceURL, methods);


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

        if (!(password == passwordAgain))
        {
            alert("两次密码输入不一�?);
            return;
        }

		var result = service.register(userName, password, type);
        if (!result.result)
        {
            alert(result.reason);
            return;
        }
        window.location.href="login.html"; 	
	}
	

    $(".reg-form-input").blur(function () 
    {
        var inputVal = $(this).val();
        if(inputVal == "")
        {
            error.call(this);
        }
    });
        
    $(".reg-form-input").focus(function () 
    {
        $(this).removeClass("error");
        $(this).next().remove(".n-msg");
        $(this).next().show();
    });

    $("#js-reg-sub").click(function () {
        var state = true;
        $(".reg-form-input").each(function()
        {
            if ($(this).val() == "")
            {
                error.call(this);
                state = false
            }
        });

    });

    $("#returntologin").click(function()
    {
        location.href = "login.html";
    });

    function error() {
        $(this).addClass("error");
        $(this).next().hide();
        var message = "<span class='n-msg' style='color: #ff0000'>" + $(this).prev().html().replace("�?, "") + "不能为空</span>"
        $(this).after(message);
    }

});


	
