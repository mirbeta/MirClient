{***************************************************************************}
{ TAdvStringGrid Memo EditLink                                              }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 1996-2011                                          }
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

unit AsgMemo;

{$I TMSDEFS.INC}

interface

uses
  AdvGrid, StdCtrls, Windows, Classes, Messages, Controls, Graphics, Forms,
  Grids, Menus
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type

  TAdvGridMemo = class(TMemo)
  private
    FTabIsExit: Boolean;
    procedure CMWantSpecialKey(var Msg:TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
  protected
  public
  published
    property TabIsExit: Boolean read FTabIsExit write FTabIsExit;
  end;

  TMemoEditLink = class(TEditLink)
  private
    FEdit: TAdvGridMemo;
    FColor: TColor;
    FMaxLength: Integer;
    FScrollbars: TScrollStyle;
    FTabIsExit: Boolean;
    FSelectAll: Boolean;
    FWordWrap: Boolean;
  protected
    procedure EditExit(Sender: TObject);
  public
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    constructor Create(AOwner: TComponent); override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    function GetEditControl: TWinControl; override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    procedure SetProperties; override;
    procedure SetCellProps(AColor: TColor; AFont: TFont); override;
  published
    property Color: TColor read FColor write FColor;
    property Scrollbars: TScrollStyle read FScrollbars write FScrollbars;
    property MaxLength: Integer read FMaxLength write FMaxLength;
    property SelectAll: Boolean read FSelectAll write FSelectAll;
    property TabIsExit: Boolean read FTabIsExit write FTabIsExit;
    property WordWrap: Boolean read FWordWrap write FWordWrap default false;
    property WantTabs;
    property WantReturns;
  end;


implementation

uses
  SysUtils;

type
  {$IFDEF DELPHIXE3_LVL}
  TScrollStyle = System.UITypes.TScrollStyle;
  {$ENDIF}

  TProtectedGrid = class(TAdvStringGrid)
  end;


{ TMemoEditLink }

Procedure TMemoEditLink.CreateEditor(AParent: TWinControl);
Begin
  FEdit := TAdvGridMemo.Create(Grid);
  FEdit.Parent := AParent;
  FEdit.OnKeyUp := EditKeyDown;
  FEdit.OnExit := EditExit;
  FEdit.BorderStyle := bsSingle;
  FEdit.Scrollbars := ssBoth;
  FEdit.Width := 0;
  FEdit.Height := 0;
End;

Procedure TMemoEditLink.DestroyEditor;
Begin
  FEdit.Free;
  FEdit := nil;
End;

Procedure TMemoEditLink.EditExit(Sender: TObject);
Begin
  inherited;
End;

Function TMemoEditLink.GetEditorValue: String;
Begin
  if Assigned(FEdit) then
    Result := FEdit.Lines.Text;
End;

procedure TMemoEditLink.SetCellProps(AColor: TColor; AFont: TFont);
begin
  inherited;
  FEdit.Color := AColor;
end;

Procedure TMemoEditLink.SetEditorValue(s: String);
Begin
  FEdit.Lines.Text := s;
  if FSelectAll then
    FEdit.SelectAll
End;

Function TMemoEditLink.GetEditControl: TWinControl;
Begin
  Result := FEdit;
End;

Constructor TMemoEditLink.Create(aOwner: TComponent);
Begin
  Inherited;
  EditStyle := esPopup;
  PopupHeight := 100;
  PopupWidth := 100;
  WantKeyReturn := True;
  WantKeyUpDown := True;
  ScrollBars := ssBoth;
  Color := clNone;
End;

Procedure TMemoEditLink.SetProperties;
begin
  Inherited;

  if FColor <> clNone then
    FEdit.Color := FColor;

  FEdit.Scrollbars := FScrollbars;
  FEdit.MaxLength := FMaxLength;
  FEdit.Font.Assign(Grid.Canvas.Font);
  FEdit.TabIsExit := FTabIsExit;
  FEdit.WantReturns := WantReturns;
  FEdit.WantTabs := WantTabs;
  FEdit.WordWrap := FWordWrap;

  if FWordWrap then
    FEdit.ScrollBars := ssNone
  else
    FEdit.ScrollBars := ssBoth;

  if  EditStyle = esInplace then
    FEdit.BorderStyle := bsNone
  else
    FEdit.BorderStyle := bsSingle;
end;

procedure TMemoEditLink.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (FTabIsExit and (Key = VK_TAB)) then
  begin
    with TprotectedGrid(Grid) do
    begin
      SendMessage(Handle,WM_SETFOCUS,0,0);
      HideInplaceEdit;
      AdvanceEdit(Col,Row,goTabs in Options,True,True,False,True);
      FEdit.Text := Cells[Col,Row];
      Key := 0;
    end;
  end;

  if ((ShortCut(Key, Shift) = VK_RETURN) and WantKeyReturn) then
  begin
    with TprotectedGrid(Grid) do
    begin
      PostMessage(Handle,WM_SETFOCUS,0,0);
      HideInplaceEdit;
      Cells[Col,Row] := FEdit.Text;
      Key := 0;
      SetFocus;
    end;
  end;

  if ((Key = VK_ESCAPE) and WantKeyEscape) then
  begin
    with TprotectedGrid(Grid) do
    begin
      FEdit.Text := OriginalCellValue;
      PostMessage(Handle,WM_SETFOCUS,0,0);
      HideInplaceEdit;
      Key := 0;
      SetFocus;
    end;
  end;

end;

{ TAdvGridMemo }


procedure TAdvGridMemo.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
end;

procedure TAdvGridMemo.WMChar(var Msg: TWMChar);
begin
  if (Msg.CharCode = VK_TAB) and FTabIsExit then
    Msg.Result := 1
  else
    inherited;
end;


end.
