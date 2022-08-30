{$INCLUDE ..\..\Compilers.inc}

unit Main;

interface

uses
  {$IFDEF D7PLUS} XPMan, {$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ImgList, MSI_Defs,
  ActnList, StdActns, ComCtrls, ToolWin, AppEvnts, ExtCtrls,
  MSI_Common;

type
  Twnd_wmie_Main = class(TForm)
    MainMenu: TMainMenu;
    ActionList: TActionList;
    File1: TMenuItem;
    ToolbarImages: TImageList;
    ApplicationEvents: TApplicationEvents;
    Close1: TMenuItem;
    acExit: TAction;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    acWinCascade: TWindowCascade;
    acWinTileHorizontal: TWindowTileHorizontal;
    acWinTileVertical: TWindowTileVertical;
    Windows1: TMenuItem;
    Cascade1: TMenuItem;
    ileHorizontally1: TMenuItem;
    ileVertically1: TMenuItem;
    N8: TMenuItem;
    acAbout: TAction;
    acCloseChild: TAction;
    acRefresh: TAction;
    N1: TMenuItem;
    Refresh1: TMenuItem;
    sb: TStatusBar;
    acHome: TAction;
    HomePage1: TMenuItem;
    N3: TMenuItem;
    acConnect: TAction;
    procedure acConnectExecute(Sender: TObject);
    procedure acCloseChildExecute(Sender: TObject);
    procedure acHomeExecute(Sender: TObject);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure acExitExecute(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
  private
  protected
  public
    procedure SetControls;

    function ShowChild(AChild :TFormClass; ChildCaption: string; ImageIdx: Integer = -1; AHint: string = ''; ABringToFront: Boolean = True; AMultiInstance: boolean = False) :TForm;
    function FindChild(AChild :TFormClass; ChildCaption: string = '') :TForm;

    procedure OpenView;
  end;

var
  wnd_wmie_Main: Twnd_wmie_Main;
  PCIDEVS, USBDEVS, MONDEVS: TStringList;

implementation

uses ShellAPI, MiTeC_Dialogs, Viewer, MiTeC_Routines, LoginDlg;

{$R *.dfm}

function Twnd_wmie_Main.FindChild(AChild: TFormClass;
  ChildCaption: string): TForm;
var
  i :integer;
begin
  Result:=nil;
  for i:=0 to mdichildcount-1 do
    if (AChild=MDIChildren[i].ClassType) and
       ((Trim(ChildCaption)='') or ((Trim(ChildCaption)<>'') and SameText(ChildCaption,MDIChildren[i].Caption))) then begin
      Result:=MDIChildren[i];
      Break;
    end;
end;

function Twnd_wmie_Main.ShowChild;
var
  i :integer;
  icon: TIcon;
begin
  Icon:=TIcon.Create;
  Result:=nil;
  if not AMultiInstance then
    for i:=0 to mdichildcount-1 do
      if (AChild=MDIChildren[i].ClassType) and SameText(ChildCaption,MDIChildren[i].Caption) then begin
        result:=MDIChildren[i];
        break;
      end;
  if assigned(result) then
    result.show
  else begin
    result:=achild.create(self);
    Result.Caption:=ChildCaption;
    Result.Icon:=icon;
  end;
  icon.Free;
end;

procedure Twnd_wmie_Main.ApplicationEventsIdle(Sender: TObject;
  var Done: Boolean);
var
  i,n: Integer;
begin
  SetControls;

  n:=MDIChildCount;
  for i:=n-1 downto 0 do
    {if (MDIChildren[i] is Twnd_adoq_Query) then
      (MDIChildren[i] as Twnd_adoq_Query).SetControls
    else if (MDIChildren[i] is Twnd_adoq_History) then
      (MDIChildren[i] as Twnd_adoq_History).SetControls;
    {else if (MDIChildren[i] is Twnd_adoq_Table) then
      (MDIChildren[i] as Twnd_adoq_Table).SetControls;}

  try

  except
  end;
end;


procedure Twnd_wmie_Main.SetControls;
begin
  {if ActiveMDIChild is Tmdi_msi_Viewer then
    (ActiveMDIChild as Tmdi_msi_Viewer).SetControls;
  acCloseChild.Enabled:=MDIChildCount>0;
  acRefresh.Enabled:=MDIChildCount>0;
  acSave.Enabled:=MDIChildCount>0;}
end;

procedure Twnd_wmie_Main.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure Twnd_wmie_Main.acAboutExecute(Sender: TObject);
begin
  ShellAbout(Handle,PChar(Application.Title+' '+cVersion),cCopyright,Application.Icon.Handle);
end;

procedure Twnd_wmie_Main.acCloseChildExecute(Sender: TObject);
begin
  ACtiveMDIChild.Close;
end;

procedure Twnd_wmie_Main.acConnectExecute(Sender: TObject);
begin
  OpenView;
end;

procedure Twnd_wmie_Main.acHomeExecute(Sender: TObject);
begin
  ShellExecute(handle,'open',cWWW,nil,nil,SW_NORMAL);
end;

procedure Twnd_wmie_Main.OpenView;
var
  Child: TForm;
  m,u,p,r,s: string;
begin
  if ShowLoginDlg(m,u,p,r) then begin
    if m='' then
      s:='.'
    else
      s:=m;
    Child:=ShowChild(Tmdi_wmie_Viewer,Format('\\%s\%s',[s,r]),5,Format('\\%s\%s',[s,r]),True,True);
    with Tmdi_wmie_Viewer(Child) do begin
      Screen.Cursor:=crHourglass;
      try
        if not Connect(m,u,p,r) then begin
          Error(Format('Cannot connect to selected namespace \\%s\%s',[s,r]));
          Close;
        end;
      finally
        Screen.Cursor:=crDefault;
      end;
    end;
  end;
end;

end.
