{***************************************************************************}
{ TMS W7 Controls Pack                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit W7Labels;

{$I TMSDEFS.INC}

interface

uses
  Classes, StdCtrls, Messages, Controls, Graphics, W7Common
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TW7ActiveLabel = class (TCustomLabel)
  private
    FMouseInControl: boolean;
    FMouseInColor: TColor;
    FMouseOutColor: TColor;
    FMouseInCursor: TCursor;
    FMouseOutCursor: TCursor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure ChangeLook;
  public
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;
  published
    property MouseInColor: TColor read FMouseInColor write FMouseInColor nodefault;
    property MouseOutColor: TColor read FMouseOutColor write FMouseOutColor nodefault;
    property MouseInCursor: TCursor read FMouseInCursor write FMouseInCursor nodefault;
    property MouseOutCursor: TCursor read FMouseOutCursor write FMouseOutCursor nodefault;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    {$IFDEF DELPHI2006_LVL}
    property EllipsisPosition;
    {$ENDIF}
    property Enabled;
    property FocusControl;
    property Font;
    {$IFDEF DELPHI_UNICODE}
    property GlowSize; // Windows Vista only
    {$ENDIF}
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    {$IFDEF DELPHI2010_LVL}
    property Touch;
    {$ENDIF}
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF DELPHI2010_LVL}
    property OnGesture;
    {$ENDIF}
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

constructor TW7ActiveLabel.Create(Aowner: TComponent);
begin
  inherited;
  FMouseInControl := False;
  FMouseInColor := $00E54A07;
  FMouseOutColor := $00551C15;
  FMouseInCursor := crHandPoint;
  FMouseOutCursor := crDefault;
  if GetWindowsVersion >= 6 then
  begin
    Font.Name := W7StandartFontName;
    Font.Size := W7StandartFontSize;
  end;
  ChangeLook;
end;

destructor TW7ActiveLabel.Destroy;
begin
  inherited;
end;

procedure TW7ActiveLabel.CMMouseEnter(var Message: TMessage);
begin
  FMouseInControl := True;
  ChangeLook;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TW7ActiveLabel.CMMouseLeave(var Message: TMessage);
begin
  FMouseInControl := False;
  ChangeLook;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TW7ActiveLabel.ChangeLook;
begin
  case FMouseInControl of
    True:
    begin
      Font.Color := FMouseInColor;
      Font.Style := Font.Style + [fsUnderline];
      Cursor := MouseInCursor;
    end;
    False:
    begin
      Font.Color := FMouseOutColor;
      Font.Style := Font.Style - [fsUnderline];
      Cursor := MouseOutCursor;
    end;
  end;
end;

end.