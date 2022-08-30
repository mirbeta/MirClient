unit NotesMainForm;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, NotepadMainForm, NotepadChildForm,
  cxFontNameComboBox, cxDropDownEdit, cxPC,
  cxPCdxBarPopupMenu, ActnList, dxTabbedMDI, dxBar, dxRibbonGallery,
  dxSkinChooserGallery, cxBarEditItem, cxLookAndFeels, ImgList, cxGraphics,
  dxRibbonRadialMenu, dxCore, cxGeometry, cxControls, Clipbrd, cxRichEditUtils, cxRichEdit,
  ExtDlgs, dxGDIPlusClasses, cxClasses;

const
  UM_SHOWMENU = WM_USER+2;

type
  TfmNotesMainForm = class(TfrmNotepadMain)
    rmFileRadialMenu: TdxRibbonRadialMenu;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    dxBarButton4: TdxBarButton;
    dxBarButton5: TdxBarButton;
    bbCloseActiveChild: TdxBarButton;
    dxBarButton6: TdxBarButton;
    rmSelection: TdxRibbonRadialMenu;
    dxBarButton8: TdxBarButton;
    dxBarButton9: TdxBarButton;
    dxBarButton10: TdxBarButton;
    dxBarButton11: TdxBarButton;
    dxBarSubItem1: TdxBarSubItem;
    dxBarButton12: TdxBarButton;
    dxBarButton13: TdxBarButton;
    dxBarButton14: TdxBarButton;
    dxBarSubItem2: TdxBarSubItem;
    dxBarButton15: TdxBarButton;
    dxBarButton16: TdxBarButton;
    dxBarSubItem3: TdxBarSubItem;
    dxBarSubItem4: TdxBarSubItem;
    dxBarButton7: TdxBarButton;
    dxBarButton17: TdxBarButton;
    dxBarButton1: TdxBarButton;
    dxBarButton18: TdxBarButton;
    rmBasic: TdxRibbonRadialMenu;
    bsiSymbol: TdxBarSubItem;
    dxBarButton19: TdxBarButton;
    dxBarButton20: TdxBarButton;
    OpenPictureDialog2: TOpenPictureDialog;
    bbClearFormat: TdxBarButton;
    procedure FormShow(Sender: TObject);
    procedure bbCloseActiveChildClick(Sender: TObject);
    procedure dxBarButton19Click(Sender: TObject);
    procedure dxBarButton20Click(Sender: TObject);
    procedure SymbolClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bbClearFormatClick(Sender: TObject);
    procedure acUnderlineExecute(Sender: TObject);
    procedure acBoldExecute(Sender: TObject);
    procedure acItalicExecute(Sender: TObject);
    procedure rmFileRadialMenuCloseUp(Sender: TObject);
  private
    FActiveRadialMenu: TdxRibbonRadialMenu;
    FRadialMenuPos: TPoint;
    FLockMenuUpdate: Integer;

    function GetMenuForShow: TdxRibbonRadialMenu;
    function GetMenuPopupPoint(const P: TPoint): TPoint;
    procedure CheckActiveMenu;
    procedure HideActiveMenu;
    procedure ShowEditorMenu(ARadialMenu: TdxRibbonRadialMenu; P: TPoint);
    procedure PostponedShowMenu(ARadialMenu: TdxRibbonRadialMenu; P: TPoint);
    procedure EditorContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MainFormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PopulateSymbols;

    procedure UMShowMenu(var Message: TMessage); message UM_SHOWMENU;
    procedure NCLButtonDown(var Message: TMessage); message WM_NCLBUTTONDOWN;
  protected
    function CreateChildForm: TfrmNotepadChild; override;
    procedure DoUpdateControls(AActiveChild: TfrmNotepadChild); override;
  public
    { Public declarations }
  end;

var
  fmNotesMainForm: TfmNotesMainForm;

implementation

uses dxRibbonForm;

{$R *.dfm}

type
  TClipboardAccess = class(TClipboard);

function TfmNotesMainForm.CreateChildForm: TfrmNotepadChild;
begin
  Result := inherited CreateChildForm;
  Result.Editor.OnMouseUp := EditorMouseUp;
  Result.Editor.OnContextPopup := EditorContextPopup;

  Editor.Properties.AllowObjects := True;
end;

procedure TfmNotesMainForm.FormShow(Sender: TObject);
var
  AChild: TForm;
begin
  inherited;
  AChild := MDIChildren[0];
  AChild.WindowState := wsMaximized;

{$IFNDEF DELPHI11}
  dxBarButton19.Enabled := False;
{$ENDIF}
end;

procedure TfmNotesMainForm.bbCloseActiveChildClick(Sender: TObject);
begin
  inherited;
  if ActiveChild <> nil then
    ActiveChild.Close;
end;

procedure TfmNotesMainForm.DoUpdateControls(
  AActiveChild: TfrmNotepadChild);
begin
  inherited;
  bbCloseActiveChild.Enabled := ActiveChild <> nil;
  bbClearFormat.Enabled := (AActiveChild <> nil) and (AActiveChild.Editor.SelLength > 0);  

  if FLockMenuUpdate = 0 then
    CheckActiveMenu;
end;

function TfmNotesMainForm.GetMenuForShow: TdxRibbonRadialMenu;
begin
  if Editor.SelLength <> 0 then
    Result := rmSelection
  else
    Result := rmBasic;
end;

function TfmNotesMainForm.GetMenuPopupPoint(const P: TPoint): TPoint;
begin
  Result := ClientToScreen(P);
  Result.X := Result.X + 200;
end;

procedure TfmNotesMainForm.CheckActiveMenu;
var
  P: TPoint;
begin
  if ActiveChild = nil then
    HideActiveMenu
  else
  begin
    GetCaretPos(P);
    PostponedShowMenu(GetMenuForShow, P);
  end;
end;

procedure TfmNotesMainForm.HideActiveMenu;
begin
  ShowEditorMenu(nil, cxInvalidPoint);
end;

procedure TfmNotesMainForm.ShowEditorMenu(ARadialMenu: TdxRibbonRadialMenu; P: TPoint);
begin
  dxMessagesController.KillMessages(Handle, UM_SHOWMENU);
  P := GetMenuPopupPoint(P);
  if (FActiveRadialMenu <> ARadialMenu) or not cxPointIsEqual(P, FRadialMenuPos) then
  begin
    if FActiveRadialMenu <> nil then
      FActiveRadialMenu.Hide;
    FActiveRadialMenu := ARadialMenu;
    FRadialMenuPos := P;
    if FActiveRadialMenu <> nil then
      FActiveRadialMenu.Show(P.X, P.Y);
  end;
end;

procedure TfmNotesMainForm.PostponedShowMenu(ARadialMenu: TdxRibbonRadialMenu; P: TPoint);
begin
  PostMessage(Handle, UM_SHOWMENU, WPARAM(ARadialMenu), dxPointToLParam(P));
end;

procedure TfmNotesMainForm.EditorContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
end;

procedure TfmNotesMainForm.EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if Button = mbRight then
  begin
    HideActiveMenu;
    P := GetMenuPopupPoint(Point(X, Y));
    rmFileRadialMenu.Popup(P.X, P.Y);
  end
  else
    CheckActiveMenu;
end;

procedure TfmNotesMainForm.MainFormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if Button = mbRight then
  begin
    P := ClientToScreen(Point(X, Y));
    rmFileRadialMenu.Popup(P.X, P.Y);
  end;
end;

procedure TfmNotesMainForm.PopulateSymbols;

  procedure AddItem(ACode: Integer);

    function CreateBitmap(const AFont: string; AChar: WideChar): TcxAlphaBitmap;
    var
      AGlyphSize: Integer;
      R: TRect;
    begin
      AGlyphSize := 24;
      R := Rect(0, 0, AGlyphSize, AGlyphSize);
      Result := TcxAlphaBitmap.CreateSize(R);
      Result.Canvas.Font.Name := AFont;
      Result.Canvas.Font.Color := $5C534C;
      Result.Canvas.Font.Size := 16;
      DrawTextW(Result.Canvas.Handle, @AChar, 1, R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
      Result.TransformBitmap(btmSetOpaque);
    end;

  var
    AItem: TdxBarButton;
    AFont: string;
    ABitmap: TcxAlphaBitmap;
  begin
    AItem := bsiSymbol.ItemLinks.AddItem(TdxBarButton).Item as TdxBarButton;
    AFont := 'Times New Roman';
    AItem.Description := AFont;
    AItem.CloseSubMenuOnClick := False;
    AItem.Tag := ACode;
    AItem.OnClick := SymbolClick;

    ABitmap := CreateBitmap(AFont, WideChar(ACode));
    try
      AItem.Glyph.Assign(ABitmap);
    finally
      ABitmap.Free;
    end;
  end;

  procedure PopulateGroup(AMap: array of Integer);
  var
    I: Integer;
  begin
    for I := Low(AMap) to High(AMap) do
      AddItem(AMap[I]);
  end;

const
  CurrencyMap: array [0..4] of Integer = ($20AC, $24, $A3, $A5, $20A3);
  GreekMap: array [0..9] of integer = ($03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7, $03B8, $03B9, $03BA);
  MathMap: array [0..7] of Integer = ($B1, $2260, $2264, $2265, $F7, $D7, $221E, $2211);
  SymbolMap: array [0..2] of Integer = ($A9, $AE, $2122);
begin
  PopulateGroup(MathMap);
  PopulateGroup(GreekMap);
  PopulateGroup(SymbolMap);
  PopulateGroup(CurrencyMap);
end;

procedure TfmNotesMainForm.UMShowMenu(var Message: TMessage);
begin
  ShowEditorMenu(TdxRibbonRadialMenu(Message.WParam), dxLParamToPoint(Message.LParam));
end;

procedure TfmNotesMainForm.NCLButtonDown(var Message: TMessage);
var
  P: TPoint;
begin
  if FActiveRadialMenu <> nil then
  begin
    P := ScreenToClient(FRadialMenuPos);
    FActiveRadialMenu.Hide;
  end;
  inherited;
  if FActiveRadialMenu <> nil then
  begin
    FRadialMenuPos := ClientToScreen(P);
    FActiveRadialMenu.Show(FRadialMenuPos.X, FRadialMenuPos.Y);
  end;
end;

procedure TfmNotesMainForm.dxBarButton19Click(Sender: TObject);
var
  ATable: TcxRichEditTableParams;
begin
  ATable := TcxRichEditTableParams.Create;
  try
    Editor.InsertTable(3, 3, ATable);
  finally
    ATable.Free;
  end;
end;

procedure cxRichEditInsertBitmap(ARichEdit: TcxRichEdit; ABitmap: TBitmap);
var
  AStream: TStream;
  AStreamModes: TcxRichEditStreamModes;
begin
  AStream := TStringStream.Create(dxBitmapToRTF(ABitmap));
  try
    AStreamModes := ARichEdit.ActiveProperties.StreamModes;
    try
      ARichEdit.ActiveProperties.StreamModes := ARichEdit.ActiveProperties.StreamModes + [resmSelection];
      AStream.Position := 0;
      ARichEdit.Lines.LoadFromStream(AStream);
    finally
      ARichEdit.ActiveProperties.StreamModes := AStreamModes;
    end;
  finally
    AStream.Free;
  end;
end;

procedure TfmNotesMainForm.dxBarButton20Click(Sender: TObject);
var
  ABitmap: TBitmap;
  AImage: TdxSmartImage;
begin
  inherited;
  if OpenPictureDialog2.Execute then
  begin
    AImage := TdxSmartImage.Create;
    try
      AImage.LoadFromFile(OpenPictureDialog2.FileName);
      ABitmap := AImage.GetAsBitmap;
      try
        cxRichEditInsertBitmap(Editor, ABitmap);
      finally
        ABitmap.Free;
      end;
    finally
      AImage.Free;
    end;
  end;
end;

procedure TfmNotesMainForm.SymbolClick(Sender: TObject);

  procedure InsertSymbol(AChar: WideChar);
  var
    S: WideString;
  begin
    with TClipboardAccess(Clipboard) do
    begin
      Open;
      try
        S := AChar;
        SetBuffer(CF_UNICODETEXT, PWideChar(S)^, (Length(S) + 1) * SizeOf(WideChar));
      finally
        Close;
      end;
    end;
    Editor.PasteFromClipboard;
  end;

begin
  Inc(FLockMenuUpdate);
  try
    InsertSymbol(WideChar((Sender as TdxBarButton).Tag));
  finally
    Dec(FLockMenuUpdate);
  end;
end;

procedure TfmNotesMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  PopulateSymbols;
  OnMouseUp := MainFormMouseUp;
end;

procedure TfmNotesMainForm.bbClearFormatClick(Sender: TObject);
begin
  inherited;
  Editor.SelAttributes.Assign(Editor.DefAttributes);
end;

procedure TfmNotesMainForm.acUnderlineExecute(Sender: TObject);
begin
  Inc(FLockMenuUpdate);
  try
    inherited;
  finally
    Dec(FLockMenuUpdate);
  end;
end;

procedure TfmNotesMainForm.acBoldExecute(Sender: TObject);
begin
  Inc(FLockMenuUpdate);
  try
    inherited;
  finally
    Dec(FLockMenuUpdate);
  end;
end;

procedure TfmNotesMainForm.acItalicExecute(Sender: TObject);
begin
  Inc(FLockMenuUpdate);
  try
    inherited;
  finally
    Dec(FLockMenuUpdate);
  end;
end;

procedure TfmNotesMainForm.rmFileRadialMenuCloseUp(Sender: TObject);
begin
  inherited;
  CheckActiveMenu;
end;

end.
