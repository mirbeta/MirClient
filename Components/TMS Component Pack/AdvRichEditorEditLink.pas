{**************************************************************************}
{ TADVSTRINGGRID - TADVRICHEDITOR EDITLINK                                 }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2015                                              }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}
unit AdvRichEditorEditLink;

interface

uses
  Windows, Messages, Classes, Controls, StdCtrls, Graphics, Forms, SysUtils,
  AdvGrid, ShellApi, AdvRichEditor, AdvRichEditorMiniHTMLIO,
  AdvRichEditorPopupToolBar, AdvToolBar;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvRichEditorEditLink = class(TEditLink)
  private
    FRichEditor: TAdvRichEditor;
    FRichEditorPopup: TAdvRichEditorPopupToolBar;
    FPopupToolBar: boolean;
    FPopupToolbarStyler: TCustomAdvToolBarStyler;
  protected
    procedure EditExit(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    function GetEditControl: TWinControl; override;
    procedure SetFocus(Value: Boolean); override;
    procedure SetProperties; override;
    procedure SetRect(r: TRect); override;
    property RichEditor: TAdvRichEditor read FRichEditor;
  published
    property PopupToolbar: boolean read FPopupToolBar write FPopupToolBar default true;
    property PopupToolbarStyler: TCustomAdvToolBarStyler read FPopupToolbarStyler write FPopupToolbarStyler;
  end;

procedure Register;

implementation

uses
  AdvUtil;

procedure Register;
begin
  RegisterComponents('TMS Grids',[TAdvRichEditorEditLink]);
end;

{ TAdvRichEditorEditLink }

constructor TAdvRichEditorEditLink.Create(AOwner: TComponent);
begin
  inherited;
  WantKeyLeftRight := True;
  WantKeyHomeEnd := True;
  WantHTML := True;
  FPopupToolBar := True;
end;

procedure TAdvRichEditorEditLink.CreateEditor(AParent: TWinControl);
begin
  inherited;
  FRichEditor := TAdvRichEditor.Create(Self);
  FRichEditor.BorderStyle := bsNone;
  FRichEditor.Parent := AParent;
  FRichEditor.OnExit := EditExit;

  FRichEditorPopup := TAdvRichEditorPopupToolBar.Create(FRichEditor);
  FRichEditor.PopupToolBar := FRichEditorPopup;
end;

destructor TAdvRichEditorEditLink.Destroy;
begin
  inherited;
end;

procedure TAdvRichEditorEditLink.DestroyEditor;
begin
  FRichEditor.Free;
  FRichEditorPopup.Free;
  inherited;
end;

procedure TAdvRichEditorEditLink.EditExit(Sender: TObject);
begin
  if Assigned(Screen.ActiveControl) then
  begin
    if Screen.ActiveControl.ClassName = 'TAdvOfficeFontSelector' then
    begin
      KeepFocus := true;
      Exit;
    end;

    if Screen.ActiveControl.ClassName = 'TAdvOfficeFontSizeSelector' then
    begin
      KeepFocus := true;
      Exit;
    end;

    KeepFocus := false;
  end;

  HideEditor;
end;

function TAdvRichEditorEditLink.GetEditControl: TWinControl;
begin
  Result := FRichEditor;
end;

function TAdvRichEditorEditLink.GetEditorValue: String;
var
  minihtmlIO: TAdvRichEditorMiniHTMLIO;
begin
  minihtmlIO := TAdvRichEditorMiniHTMLIO.Create(Self);
  try
    minihtmlIO.RichEditor := FRichEditor;
    Result := StripCRLF(minihtmlIO.AsString);
  finally
    minihtmlIO.Free;
  end;
end;

procedure TAdvRichEditorEditLink.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = PopupToolbarStyler) then
    FPopupToolbarStyler := nil;
end;

procedure TAdvRichEditorEditLink.SetEditorValue(s: String);
var
  minihtmlIO: TAdvRichEditorMiniHTMLIO;
begin
  minihtmlIO := TAdvRichEditorMiniHTMLIO.Create(Self);
  try
    minihtmlIO.RichEditor := FRichEditor;
    minihtmlIO.Load(s, Grid.GridImages);
  finally
    minihtmlIO.Free;
  end;
end;

procedure TAdvRichEditorEditLink.SetFocus(Value: Boolean);
begin
  inherited;

  if FPopupToolBar then
    FRichEditor.PopupToolBar := FRichEditorPopup
  else
    FRichEditor.PopupToolBar := nil;

  FRichEditorPopup.ToolBarStyler := FPopupToolbarStyler;
end;

procedure TAdvRichEditorEditLink.SetProperties;
begin
  inherited;
end;

procedure TAdvRichEditorEditLink.SetRect(r: TRect);
begin
  inherited;
end;

end.
