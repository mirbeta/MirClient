unit EDScript;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,INIFILES;

type
  TCPUID	= array[1..4] of Longint;

  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Memo1: TMemo;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
  function GetScriptText(Msg:PChar;LoMsg:PChar;LoMsgLen:Integer):integer;stdcall;

var
  Form1: TForm1;

Const
  RegName  = '作者:Development';
  PlugName = 'LEGEND脚本加密插件(2019/02/03)';
  LoadPlus = '正在加载LEGEND脚本加密插件....';
  LoadP    = 'legendm2 des codes'; //加解密密码
  ProcName = 'PlugOfScript';
  INIFile = '.\!Setup.txt';
  EDIT     = 'D1D37F19E195C5FDBCEDC5B2FF6B25EB4C9F910AEC359831';
  EDITWEB  = '3906299E1B79E517A2A856DEFF1DBD54A12F9CAD6011EB7D';

implementation
Uses
DES;

{$R *.dfm}

function GetScriptText(Msg:PChar;LoMsg:PChar;LoMsgLen:Integer):integer;stdcall;
var
  sMsg:String;
begin
  Result:=0;
  Try
    sMsg:=DecryStrHex(Msg,LoadP);
    Move(sMsg[1],LoMsg^,Length(sMsg)+1);
    Result:=Length(sMsg);
  Except
  end;
end;



procedure TForm1.BitBtn1Click(Sender: TObject);
var
  INI:TINIFILE;
begin
  INI:=TINIFile.Create(INIFile);
  Try
    INI.WriteString('WxScript','Regs',Memo1.Lines.GetText);
  Finally
    INI.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Label1.Caption:=DecryStrHex(EDIT,RegName);
  Label2.Caption:=DecryStrHex(EDITWEB,RegName);
end;

end.
