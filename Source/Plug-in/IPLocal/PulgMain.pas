//=============================================================================
//���ú���˵��:
//   �������ֵ����������̨��:
//     procedure MainOutMessasge(sMsg:String;nMode:integer)
//     sMsg ΪҪ���͵��ı�����
//     nMode Ϊ����ģʽ��0Ϊ�����ڿ���̨����ʾ��1Ϊ������ʾ���У��Ժ���ʾ
//
//   ȡ��0-255���������ɫ
//     function GetRGB(bt256:Byte):TColor;
//     bt256 Ҫ��ѯ����
//     ����ֵ Ϊ�������ɫ
//
//   ���͹㲥���֣�
//     procedure SendBroadCastMsg(sMsg:String;MsgType:TMsgType);
//     sMsg Ҫ���͵�����
//     MsgType ��������
//=============================================================================
unit PlugMain;

interface
uses
  Windows, Graphics, SysUtils, PlugManage;
procedure InitPlug(AppHandle:THandle);
procedure UnInitPlug();
function  DeCodeText(sText:String):String;
function  SearchIPLocal(sIPaddr:String):String;

implementation

uses QQWry, Share;
//=============================================================================
//���ز��ģ��ʱ���õĳ�ʼ������
//������Apphandle Ϊ��������
//=============================================================================
procedure InitPlug(AppHandle:THandle);
begin

end;
//=============================================================================
//�˳����ģ��ʱ���õĽ�������
//=============================================================================
procedure UnInitPlug();
begin
{
  д����Ӧ�������;
}
  MainOutMessasge(sUnLoadPlug,0);
end;
//=============================================================================
//��Ϸ��־��Ϣ������
//����ֵ��True ��������Ĭ����Ϸ��־��������False ����Ĭ����Ϸ��־������
//=============================================================================
function GameDataLog(sLogMsg:String):Boolean;
begin
{
  д����Ӧ������Ϸ��־����;
}
  Result:=False;
end;

//=============================================================================
//��Ϸ�ı�������Ϣ���뺯��(һ�����ڼӽ��ܽű�)
//������sText ΪҪ������ַ���
//����ֵ�����ؽ������ַ���(���ص��ַ������Ȳ��ܳ���1024�ֽڣ��������������)
//=============================================================================
function DeCodeText(sText:String):String;
begin

end;

//=============================================================================
//IP���ڵز�ѯ����
//������sIPaddr ΪҪ��ѯ��IP��ַ
//����ֵ������IP���ڵ��ı���Ϣ(���ص��ַ������Ȳ��ܳ���255�ֽڣ������ᱻ�ض�)
//=============================================================================

function SearchIPLocal(sIPaddr:String):String;
var
  QQWry: TQQWry;
begin
  try
    QQWry:=TQQWry.Create(sIPDataFileName);
    Result:=QQWry.GetIPMsg(QQWry.GetIPRecordID(sIPaddr))[2]+QQWry.GetIPMsg(QQWry.GetIPRecordID(sIPaddr))[3];
    QQWry.Free;
  except
    Result:='No Find';
  end;
end;

end.
