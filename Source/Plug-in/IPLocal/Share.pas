unit Share;

interface
const
  sPlugName        = 'ASPM2引擎IPLocal插件(2020/07/16)';
  sStartLoadPlug   = '加载ASPM2引擎 IP查询插件成功';
  sUnLoadPlug      = '卸载ASPM2引擎 IP查询插件成功';

  sIPFileName      ='.\qqwry.dat';
//设置本插件接管那些函数(数值设置0，1)
  HookDeCodeText    = 0; //文本配置信息解码函数
  HookSearchIPLocal = 1; //IP所在地查询函数


implementation

end.
