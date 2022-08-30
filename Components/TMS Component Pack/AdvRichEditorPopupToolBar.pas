{*************************************************************************}
{ TMS TAdvRichEditorToolBarPopup                                          }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2015                                              }
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

unit AdvRichEditorPopupToolBar;

interface

uses
  Classes, Graphics, Windows, StdCtrls, Forms, Messages, Controls,
  AdvRichEditor, AdvToolBarPopup, AdvToolBar;

type
  TAdvRichEditorPopupToolBarWindow = class(TAdvPopupToolBarWindow)
  private
    FRichEditor: TAdvRichEditor;
  protected
    function DoFontStyle(AType: TFontStyleType): boolean; override;
    procedure DoFontSize(ASize: integer); override;
    procedure DoFont(AName: string); override;
    procedure DoFontColor(AColor: TColor); override;
    procedure DoColor(AColor: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    property RichEditor: TAdvRichEditor read FRichEditor write FRichEditor;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvRichEditorPopupToolBar = class(TAdvRichEditorPopup)
  private
    FPopupWindow: TAdvRichEditorPopupToolBarWindow;
    FToolBarStyler: TCustomAdvToolBarStyler;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show(PT: TPoint); override;
    procedure Hide; override;
    function MouseInPopup(PT: TPoint): boolean; override;
  published
    property ToolBarStyler: TCustomAdvToolBarStyler read FToolBarStyler write FToolBarStyler;
  end;

implementation

{ TPopupToolBar }

constructor TAdvRichEditorPopupToolBarWindow.Create(AOwner: TComponent);
begin
  Options := [oBold,oItalic,oUnderline,oStrikeThrough,oSubscript, oSuperScript, oColor,oFontColor];
  inherited;
end;

procedure TAdvRichEditorPopupToolBarWindow.DoColor(AColor: TColor);
begin
  FRichEditor.SetSelectionBkColor(AColor);
end;

procedure TAdvRichEditorPopupToolBarWindow.DoFont(AName: string);
begin
  FRichEditor.SetSelectionFontName(AName);
  SetActiveWindow(FRichEditor.Parent.Handle);
  FRichEditor.SetFocus;
end;

procedure TAdvRichEditorPopupToolBarWindow.DoFontColor(AColor: TColor);
begin
  FRichEditor.SetSelectionColor(AColor);
end;

procedure TAdvRichEditorPopupToolBarWindow.DoFontSize(ASize: integer);
begin
  FRichEditor.SetSelectionFontSize(ASize);
  SetActiveWindow(FRichEditor.Parent.Handle);
  FRichEditor.SetFocus;
end;

function TAdvRichEditorPopupToolBarWindow.DoFontStyle(
  AType: TFontStyleType): boolean;
begin

  case AType of
    fstBold:
      begin
        FRichEditor.SetSelectionBold(not FRichEditor.IsSelectionBold);
        Result := FRichEditor.IsSelectionBold;
      end;
    fstItalic:
      begin
        FRichEditor.SetSelectionItalic(not FRichEditor.IsSelectionItalic);
        Result := FRichEditor.IsSelectionUnderline;
      end;
    fstUnderline:
      begin
        FRichEditor.SetSelectionUnderline(not FRichEditor.IsSelectionUnderline);
        Result := FRichEditor.IsSelectionUnderline;
      end;
    fstStrikeThrough:
      begin
        FRichEditor.SetSelectionStrikeOut(not FRichEditor.IsSelectionStrikeOut);
        Result := FRichEditor.IsSelectionStrikeOut;
      end;
    fstSubScript:
      begin
        FRichEditor.SetSelectionSubscript(not FRichEditor.IsSelectionSubscript);
        Result := FRichEditor.IsSelectionSubscript;
      end;
    fstSuperScript:
      begin
        FRichEditor.SetSelectionSuperscript(not FRichEditor.IsSelectionSuperscript);
        Result := FRichEditor.IsSelectionSuperscript;
      end
  else
    Result := false;
  end;

  SetActiveWindow(FRichEditor.Parent.Handle);
  FRichEditor.SetFocus;
end;

{ TAdvRichEditorPopupToolBar }

constructor TAdvRichEditorPopupToolBar.Create(AOwner: TComponent);
begin
  inherited;
  FPopupWindow := TAdvRichEditorPopupToolBarWindow.Create(Self);
end;

destructor TAdvRichEditorPopupToolBar.Destroy;
begin
  inherited;
end;

procedure TAdvRichEditorPopupToolBar.Hide;
begin
  inherited;
  FPopupWindow.Visible := false;
  FPopupWindow.Parent := nil;
end;

function TAdvRichEditorPopupToolBar.MouseInPopup(PT: TPoint): boolean;
begin
  Result := PtInRect(Rect(FPopupWindow.Left, FPopupWindow.Top, FPopupWindow.Left + FPopupWindow.Width, FPopupWindow.Top + FPopupWindow.Height), PT);
end;

procedure TAdvRichEditorPopupToolBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FToolBarStyler) then
    FToolBarStyler := nil;
end;

procedure TAdvRichEditorPopupToolBar.Show(PT: TPoint);
begin
  inherited;
  FPopupWindow.Left := PT.X;
  FPopupWindow.Top := PT.Y;
  FPopupWindow.Parent := RichEditor;
  FPopupWindow.RichEditor := RichEditor;
  FPopupWindow.ToolBarStyler := ToolBarStyler;

  // initialize to current richeditor state
  FPopupWindow.SetFontStyleState(fstBold, RichEditor.IsSelectionBold);
  FPopupWindow.SetFontStyleState(fstItalic, RichEditor.IsSelectionItalic);
  FPopupWindow.SetFontStyleState(fstUnderline, RichEditor.IsSelectionUnderline);
  FPopupWindow.SetFontStyleState(fstStrikeThrough, RichEditor.IsSelectionStrikeOut);
  FPopupWindow.SetFontStyleState(fstSubScript, RichEditor.IsSelectionSubScript);
  FPopupWindow.SetFontStyleState(fstSuperScript, RichEditor.IsSelectionSuperScript);

  FPopupWindow.SetFontColor(RichEditor.GetSelectionTextColor);
  FPopupWindow.SetColor(RichEditor.GetSelectionBkColor);
  FPopupWindow.SetFontSize(RichEditor.GetSelectionFontSize);
  FPopupWindow.SetFont(RichEditor.GetSelectionFontName);

  FPopupWindow.Visible := true;
  FPopupWindow.Width :=  FPopupWindow.Width + 1;
  FPopupWindow.Width :=  FPopupWindow.Width - 1;
end;

end.
