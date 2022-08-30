{*************************************************************************}
{ TMS ToolBars component                                                  }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2006 - 2013                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvThemes;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, Messages, Graphics, AdvTBXPVS, CommCtrl, Controls 
  {$IFDEF DELPHI2006_LVL}
  , uxtheme
  {$ENDIF}  
  ;

type

  TThemedElement = (
    teButton,
    teClock,
    teComboBox,
    teEdit,
    teExplorerBar,
    teHeader,
    teListView,
    teMenu,
    tePage,
    teProgress,
    teRebar,
    teScrollBar,
    teSpin,
    teStartPanel,
    teStatus,
    teTab,
    teTaskBand,
    teTaskBar,
    teToolBar,
    teToolTip,
    teTrackBar,
    teTrayNotify,
    teTreeview,
    teWindow
  );
  
  // 'Edit' theme data
  TThemedEdit = (
    teEditDontCare,
    teEditRoot,
    teEditTextNormal, teEditTextHot, teEditTextSelected, teEditTextDisabled, teEditTextFocused, teEditTextReadOnly, teEditTextAssist,
    teEditCaret
  );

  TThemeData = array[TThemedElement] of HTHEME;

  PThemedElementDetails = ^TThemedElementDetails;
  TThemedElementDetails = record
    Element: TThemedElement;
    Part,
    State: Integer;
  end;

  TThemeServices = class(TObject)
  private
    FNewComCtrls,
    FThemesAvailable,
    FUseThemes: Boolean;
    FThemeData: TThemeData;
    FOnThemeChange: TNotifyEvent;
    function GetTheme(Element: TThemedElement): HTHEME;
    function GetThemesEnabled: Boolean;
  protected
    procedure DoOnThemeChange; virtual;
    procedure UnloadThemeData;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ApplyThemeChange;
    procedure UpdateThemes;
    function GetElementDetails(Detail: TThemedEdit): TThemedElementDetails; overload;
    property Theme[Element: TThemedElement]: HTHEME read GetTheme;
    property ThemesAvailable: Boolean read FThemesAvailable;
    property ThemesEnabled: Boolean read GetThemesEnabled;
    property OnThemeChange: TNotifyEvent read FOnThemeChange write FOnThemeChange;
  end;

  TThemeServicesClass = class of TThemeServices;

function ThemeServices: TThemeServices;

var
  ThemeServicesClass: TThemeServicesClass = TThemeServices;

implementation

uses
  SysUtils, ComCtrls;

const
  ThemeDataNames: array[TThemedElement] of PWideChar = (
    'button',      // teButton
    'clock',       // teClock
    'combobox',    // teComboBox
    'edit',        // teEdit
    'explorerbar', // teExplorerBar
    'header',      // teHeader
    'listview',    // teListView
    'menu',        // teMenu
    'page',        // tePage
    'progress',    // teProgress
    'rebar',       // teRebar
    'scrollbar',   // teScrollBar
    'spin',        // teSpin
    'startpanel',  // teStartPanel
    'status',      // teStatus
    'tab',         // teTab
    'taskband',    // teTaskBand
    'taskbar',     // teTaskBar
    'toolbar',     // teToolBar
    'tooltip',     // teToolTip
    'trackbar',    // teTrackBar
    'traynotify',  // teTrayNotify
    'treeview',    // teTreeview
    'window'       // teWindow
  );

var
  InternalServices: TThemeServices;

//------------------------------------------------------------------------------

function ThemeServices: TThemeServices;
begin
  if InternalServices = nil then
    InternalServices := ThemeServicesClass.Create;
  Result := InternalServices;
end;

//------------------------------------------------------------------------------

constructor TThemeServices.Create;
begin
  FThemesAvailable := InitThemeLibrary;
  FNewComCtrls := GetComCtlVersion >= ComCtlVersionIE6;
  UpdateThemes;
end;

//------------------------------------------------------------------------------

destructor TThemeServices.Destroy;
begin
  UnloadThemeData;
  FreeThemeLibrary;
  inherited;
end;

//------------------------------------------------------------------------------

function TThemeServices.GetTheme(Element: TThemedElement): HTHEME;
begin
  if FUseThemes and (FThemeData[Element] = 0) then
    FThemeData[Element] := OpenThemeData(0, ThemeDataNames[Element]);
  Result := FThemeData[Element];
end;

//------------------------------------------------------------------------------

function TThemeServices.GetThemesEnabled: Boolean;
begin
  Result := FThemesAvailable and FUseThemes and FNewComCtrls;
end;

//------------------------------------------------------------------------------

procedure TThemeServices.DoOnThemeChange;
begin
  if Assigned(FOnThemeChange) then
    FOnThemeChange(Self);
end;

//------------------------------------------------------------------------------

procedure TThemeServices.UnloadThemeData;
var
  Entry: TThemedElement;
begin
  for Entry := Low(TThemeData) to High(TThemeData) do
    if FThemeData[Entry] <> 0 then
    begin
      CloseThemeData(FThemeData[Entry]);
      FThemeData[Entry] := 0;
    end;
end;

//------------------------------------------------------------------------------

procedure TThemeServices.ApplyThemeChange;
begin
  UpdateThemes;
  DoOnThemeChange;
end;

//------------------------------------------------------------------------------

procedure TThemeServices.UpdateThemes;
begin
  if FUseThemes then
    UnloadThemeData;
  FUseThemes := UseThemes;
end;

//------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedEdit): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teEdit;
  with Result do
  begin
    case Detail of
      teEditTextNormal..teEditTextAssist:
        begin
          Part := EP_EDITTEXT;
          Base := Ord(teEditTextNormal);
        end;
      teEditCaret:
        begin
          Part := EP_CARET;
          Base := Ord(teEditCaret);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//------------------------------------------------------------------------------


initialization
finalization
  InternalServices.Free;
end.
