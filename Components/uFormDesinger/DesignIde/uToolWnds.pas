{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit uToolWnds;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  uDesignWindows, ExtCtrls, ToolWin, ComCtrls, IniFiles, ActnListXE, Menus,
  ActnPopup, PlatformDefaultStyleActnCtrls, System.Actions, Vcl.ActnList;

type
  TToolbarDesignWindow = class(TDesignWindow)
    ToolBar1: TToolBar;
    Splitter1: TSplitter;
    PopupMenu1: TPopupActionBar;
    ActionList1: TActionList;
    ToolbarCmd: TAction;
    TextLabelsCmd: TAction;
    Toolbar2: TMenuItem;
    PopupMenu2: TPopupActionBar;
    TextLabels1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure ToolbarCmdExecute(Sender: TObject);
    procedure TextLabelsCmdExecute(Sender: TObject);
    procedure ToolbarCmdUpdate(Sender: TObject);
    procedure TextLabelsCmdUpdate(Sender: TObject);
  private
    FLargeButtons: Boolean;
    function GetLargeButtons: Boolean;
    procedure SetLargeButtons(Value: Boolean);
  protected
    procedure ResizeButtons(Large: Boolean); virtual;
  public
    property LargeButtons: Boolean read GetLargeButtons write SetLargeButtons;
  end;

implementation

{$R *.dfm}

{
  Simple Intl fix to display longer captions on localized versions of the
  product.        
  InitFromResources is called in initialization section.
  See bug #105175
}

resourcestring
  sSmallToolbarSize = '30';
  sSmallButtonHeight = '22';
  sSmallButtonWidth = '23';
  sLargeToolbarSize = '44';
  sLargeButtonHeight = '36';
  sLargeButtonWidth = '56';

var
  SmallToolbarSize: Integer;
  SmallButtonHeight: Integer;
  SmallButtonWidth: Integer;
  LargeToolbarSize: Integer;
  LargeButtonHeight: Integer;
  LargeButtonWidth: Integer;

procedure InitFromResources;
begin
  SmallToolbarSize := StrToIntDef(sSmallToolbarSize, 30);
  SmallButtonHeight := StrToIntDef(sSmallButtonHeight, 22);
  SmallButtonWidth := StrToIntDef(sSmallButtonWidth, 23);
  LargeToolbarSize := StrToIntDef(sLargeToolbarSize, 44);
  LargeButtonHeight := StrToIntDef(sLargeButtonHeight, 36);
  LargeButtonWidth := StrToIntDef(sLargeButtonWidth, 56);
end;

procedure TToolbarDesignWindow.FormCreate(Sender: TObject);
{+
var
  Control: TControl;

  function RightMostControl: TControl;
  var
	I: Integer;
	Control: TControl;
  begin
	Result := nil;
	with ToolBar1 do
	  for I := 0 to ControlCount - 1 do
	  begin
		Control := Controls[I];
		if (Result = nil) or (Control.Left > Result.Left) then
		  Result := Control;
	  end;
  end;
!}
begin
  // Make sure default window size contains all speed buttons
{+ 
  Control := RightMostControl;
  if Control <> nil then
    if Control.Left + Control.Width > ToolBar1.Width then
      Width := Width + Control.Left + Control.Width - ToolBar1.Width;
!}      
//!  ResizeButtons(AppIniFile.ReadBool(isGlobals, ivTextLabels, False));
  { Toggle to force update }
  FLargeButtons := not LargeButtons;
  LargeButtons := not FLargeButtons;
end;

function TToolbarDesignWindow.GetLargeButtons: Boolean;
begin
  Result := ToolBar1.Height > SmallToolBarSize;
end;

procedure TToolbarDesignWindow.SetLargeButtons(Value: Boolean);
begin
  ResizeButtons(Value);
end;

procedure TToolbarDesignWindow.Splitter1CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  with Toolbar1 do
    if (Height >= LargeToolbarSize) then
      if (NewSize <= SmallToolbarSize) then
        NewSize := SmallToolbarSize
      else
        NewSize := LargeToolbarSize
    else
      if(NewSize >= LargeToolbarSize) then
        NewSize := LargeToolbarSize
      else
        NewSize := SmallToolbarSize;
end;

procedure TToolbarDesignWindow.Splitter1Moved(Sender: TObject);
begin
  ResizeButtons(ToolBar1.Height >= LargeToolbarSize);
end;

procedure TToolbarDesignWindow.ResizeButtons(Large: Boolean);
var
//  I: Integer;
  NewLargeWidth, NewLargeHeight: Integer;
begin
  if Large <> FLargeButtons then
  begin
    with ToolBar1 do
    begin
      Perform(WM_SETREDRAW, 0, 0);
      try
        if Large then
        begin
          NewLargeWidth := MulDiv(LargeButtonWidth, PixelsPerInch, 96);
          NewLargeHeight := LargeButtonHeight + MulDiv(13{Height of text}, PixelsPerInch, 96) - 13;
              { Large buttons }
{
          for I := 0 to ButtonCount - 1 do
            if not (Buttons[I].Style in [tbsSeparator, tbsDivider]) then
              Buttons[I].AutoSize := True;
}
          ShowCaptions := True;
          ButtonWidth := NewLargeWidth;
          ButtonHeight := NewLargeHeight;
          //! Take into account large font systems
          Height := ButtonHeight + 8;//!LargeToolbarSize;
          ShowHint := False;
        end
        else
        begin
          { Small buttons }
{
          for I := 0 to ButtonCount - 1 do
            if not (Buttons[I].Style in [tbsSeparator, tbsDivider]) then
              Buttons[I].AutoSize := False;
}
          ShowCaptions := False;
          ButtonWidth := SmallButtonWidth;
          ButtonHeight := SmallButtonHeight;
          Height := SmallToolbarSize;
          ShowHint := True;
        end;
      finally
        Perform(WM_SETREDRAW, 1, 0);
        Invalidate;
      end;
    end;
    FLargeButtons := Large;
  end;
end;

procedure TToolbarDesignWindow.ToolbarCmdExecute(Sender: TObject);
begin
  with ToolbarCmd do
  begin
    Splitter1.Top := Toolbar1.Top + Toolbar1.Height;
    ToolBar1.Visible := not Checked;
    Splitter1.Visible := Toolbar1.Visible;
  end;
end;

procedure TToolbarDesignWindow.TextLabelsCmdExecute(Sender: TObject);
begin
  with TextLabelsCmd do
    LargeButtons := not Checked;
end;

procedure TToolbarDesignWindow.ToolbarCmdUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := Toolbar1.Visible;
end;

procedure TToolbarDesignWindow.TextLabelsCmdUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := LargeButtons;
end;   

initialization
  InitFromResources;

end.
