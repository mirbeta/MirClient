unit uCpuIDMain;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ�TCnCpuId ��ʾ����
* ��Ԫ���ߣ�SkyJacker(HeMiaoYu@gmail.com)
* ��    ע��
* ����ƽ̨��WinXP sp2 + Delphi 6.0 up2
* ���ݲ��ԣ���
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��Ԫ��ʶ��$Id: uCpuIDMain.pas,v 1.5 2008/08/01 10:48:56 liuxiao Exp $
* �޸ļ�¼��2007.01.23
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFrmCPUIDs = class(TForm)
    BtnGetCpuIDs: TButton;
    Memo1: TMemo;
    btnGetUsage: TButton;
    btnGetCpuOems: TButton;
    btnInfoStr: TButton;
    btnGetBios: TButton;
    procedure BtnGetCpuIDsClick(Sender: TObject);
    procedure btnGetUsageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGetCpuOemsClick(Sender: TObject);
    procedure btnInfoStrClick(Sender: TObject);
    procedure btnGetBiosClick(Sender: TObject);
  private
    { Private declarations }
    procedure log(const Info: string);
  public
    { Public declarations }
  end;

var
  FrmCPUIDs: TFrmCPUIDs;

implementation

uses
  CnHardWareInfo;

var
  CnCpuID: TCnCpuID;
  
{$R *.dfm}

procedure TFrmCPUIDs.log(const Info: string);
begin
  memo1.Lines.Add(Info);
end;

procedure TFrmCPUIDs.BtnGetCpuIDsClick(Sender: TObject);
var
  I: Integer;
begin
  for I:=0 to CnCpuID.CPUCount-1 do
    if CnCpuID.SupportCPUId[I] then
      Log('�� '+IntToStr(I)+' �� CPU ֧�� cpuid ָ�')
    else
      Log('�� '+IntToStr(I)+' �� CPU ��֧�� cpuid ָ�');

  Log('');
  for I:=0 to CnCpuID.CPUCount-1 do
    if CnCpuID.SupportCPUSn[I] then
      Log('�� '+IntToStr(I)+' �� CPU ֧�ֶ�ȡ���кš�')
    else
      Log('�� '+IntToStr(I)+' �� CPU ��֧�ֶ�ȡ���кš�');

  //Ĭ����ʽ
  Log('');
  Log('Ĭ����ʽ');
  Log('CPU�ĸ�����'+IntToStr(CnCpuID.CPUCount));
  Log('����CPU��'+CnCpuID.FirstCPUId);

  for I:=0 to CnCpuID.CPUCount-1 do
  begin
    Log('�� '+IntToStr(I)+' �� CPU�����кţ�'+CnCpuID.CPUId[I]);
  end;

  //�޸���ʾ��ʽ
  Log('');
  Log('�ָ�����ʽ');
  CnCpuID.CPUIdFormat := ifDashed;
  Log('CPU�ĸ�����'+IntToStr(CnCpuID.CPUCount));
  Log('����CPU��'+CnCpuID.FirstCPUId);

  for I:=0 to CnCpuID.CPUCount-1 do
  begin
    Log('�� '+IntToStr(I)+' �� CPU�����кţ�'+CnCpuID.CPUId[I]);
  end;
end;

procedure TFrmCPUIDs.btnGetUsageClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to CnCpuID.CPUCount - 1 do
  begin
    Log('�� '+IntToStr(I)+' �� CPU��ռ���ʣ�'+ InttoStr(CnCpuID.CPUUsage[I]));
  end;
  Log('CPUƽ��ռ���ʣ�'+ InttoStr(CnCpuID.AverageCPUUsage));
  Log('');
end;

procedure TFrmCPUIDs.FormCreate(Sender: TObject);
begin
  CnCpuID := TCnCpuId.Create;
end;

procedure TFrmCPUIDs.FormDestroy(Sender: TObject);
begin
  CnCpuID.Free;
end;

procedure TFrmCPUIDs.btnGetCpuOemsClick(Sender: TObject);
var
  I: Integer;
begin
  Log('');
  for I := 0 to CnCpuID.CPUCount-1 do
    Log('�� '+IntToStr(I)+' �� CPU���������̣�'+CnCpuID.CPUOem[I]);
  Log('');
end;

procedure TFrmCPUIDs.btnInfoStrClick(Sender: TObject);
var
  I: Integer;
begin
  //�޸���ʾ��ʽ
  Log('');
  Log('�ָ�����ʽ');
  CnCpuID.CPUIdFormat := ifDashed;
  Log('CPU�ĸ�����'+IntToStr(CnCpuID.CPUCount));
  Log('����CPU��'+CnCpuID.FirstCPUInfoString);

  for I:=0 to CnCpuID.CPUCount-1 do
  begin
    Log('�� '+IntToStr(I)+' �� CPU����Ϣ����'+CnCpuID.CPUInfoString[I]);
  end;
end;

procedure TFrmCPUIDs.btnGetBiosClick(Sender: TObject);
begin
  Log(CnGetBiosID);
end;

end.
