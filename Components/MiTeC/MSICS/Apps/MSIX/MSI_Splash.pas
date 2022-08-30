{$INCLUDE ..\..\Compilers.inc}

unit MSI_Splash;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.Messages, VCL.Graphics, VCL.Controls,
     VCL.Forms, VCL.StdCtrls, VCL.Dialogs, VCL.Menus, VCL.ExtCtrls, VCL.ComCtrls;
     {$ELSE}
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
     Dialogs, Menus, ImgList, ComCtrls, ExtCtrls, StdCtrls;
     {$ENDIF}

type
  TscrMSI_Splash = class(TForm)
    Panel1: TPanel;
    Panel3: TPanel;
    lCopy: TLabel;
    lVersion: TLabel;
    Image2: TImage;
    lProd: TLabel;
    Icon: TImage;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure FallFromTop;
    procedure FallDown;
  end;

var
  scrMSI_Splash: TscrMSI_Splash;

procedure ShowSplash;
procedure HideSplash;

implementation

uses MSI_Common, MiTeC_Routines;

{$R *.DFM}

procedure ShowSplash;
begin
  if IsConsole then
    Exit;
  if WinControlExists(scrMSI_Splash) then
    scrMSI_Splash.Update
  else begin
    scrMSI_Splash:=TscrMSI_Splash.Create(nil);
    scrMSI_Splash.Show;
    scrMSI_Splash.Update
    //scrMSI_Splash.FallFromTop;
  end;
end;

procedure HideSplash;
begin
  if IsConsole then
    Exit;
  if WinControlExists(scrMSI_Splash) then
    //scrMSI_Splash.FallDown;
    FreeAndNil(scrMSI_Splash);
end;

procedure TscrMSI_Splash.FallDown;
var
  i: Integer;
  x: Double;
begin
  x:=1;
  i:=Top;
  repeat
    Top:=i;
    Inc(i,Round(x));
    x:=x+0.02;
    Update;
  until Top>Screen.Height;
end;

procedure TscrMSI_Splash.FallFromTop;
var
  i,c,t: Integer;
  x: Double;
begin
  Top:=-Height;
  i:=0;
  x:=1;
  repeat
    Top:=i;
    Update;
    Inc(i,Round(x));
    x:=x+0.02;
  until i>(Screen.Height-Height) div 2;
  x:=0.1;
  t:=Top;
  repeat
    c:=Round(100*sin(x)/x);
    Top:=t+c;
    Update;
    x:=x+1;
    sleep(20);
  until x>20;
end;

procedure TscrMSI_Splash.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  FormStyle:=fsNormal;
  {$ENDIF}
  Top:=-Height;
  lProd.Caption:=ModuleInfo.ProductName;
  lVersion.Caption:=ModuleInfo.FileVersion;
  lCopy.Caption:=ModuleInfo.Copyright;
  Icon.Picture.Icon.Handle:=Application.Icon.Handle;

  {$IFDEF THEMESUPPORT}
  Panel1.ParentBackground:=False;
  Panel3.ParentBackground:=False;
  {$ENDIF}
end;

end.
