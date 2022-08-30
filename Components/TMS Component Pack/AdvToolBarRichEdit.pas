{*************************************************************************}
{ TMS TAdvRichEditor toolbar                                              }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2014                                              }
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
unit AdvToolBarRichEdit;

interface

uses
  Classes, AdvToolBar, AdvToolBarExt, Graphics, ComCtrls,
  ActnList, AdvOfficeSelectors, AdvOfficeComboBox, AdvGlowButton, SysUtils,
  StdActns, ExtActns, RichEdit;

type
  TActionClass = class of TAction;

  TRichEditTextColorAction = class(TAction)
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TRichEditColorAction = class(TAction)
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TRichEditFontNameAction = class(TAction)
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TRichEditFontSizeAction = class(TAction)
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TRichEditToolBar = class(TCustomAdvToolBar)
  private
    FRichEdit: TRichEdit;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure SelectionBullets(Sender: TObject);
    procedure SelectBullet(Sender: TObject; Index: Integer; Item: TAdvSelectorItem);
    procedure SelectionNumbering(Sender: TObject);
    procedure SelectFontName(Sender: TObject; AName: string);
    procedure SelectFontSize(Sender: TObject; ASize: integer);
    procedure SelectColor(Sender: TObject; AColor: TColor);
    procedure SelectTextColor(Sender: TObject; AColor: TColor);
    procedure ExitFontSize(Sender: TObject);
    procedure ExitFontName(Sender: TObject);
  public
  published
    property RichEdit: TRichEdit read FRichEdit write FRichEdit;
  end;

  // Classic docking toolbars

  TRichEditToolBarFormatButton = (btBold, btItalic, btUnderline, btStrikeThrough,
    btBullet, btTextColor, btBackgroundColor, btAlignLeft, btAlignCenter,
    btAlignRight);

  TRichEditToolBarFormatButtons = set of TRichEditToolBarFormatButton;

  TRichEditFormatHints = class(TPersistent)
  private
    FNumberedBulletContent: string;
    FUnderlineContent: string;
    FSubScriptContent: string;
    FItalicTitle: string;
    FInsertHyperlinkTitle: string;
    FBackgroundColorTitle: string;
    FBulletTitle: string;
    FAlignCenterTitle: string;
    FAlignLeftTitle: string;
    FBoldTitle: string;
    FIndentTitle: string;
    FStrikeThroughTitle: string;
    FUnIndentTitle: string;
    FInsertSpecialCharTitle: string;
    FInsertPictureTitle: string;
    FAlignRightTitle: string;
    FSuperScriptTitle: string;
    FItalicContent: string;
    FInsertHyperlinkContent: string;
    FBackgroundColorContent: string;
    FTextColorTitle: string;
    FBulletContent: string;
    FAlignCenterContent: string;
    FAlignLeftContent: string;
    FNumberedBulletTitle: string;
    FBoldContent: string;
    FIndentContent: string;
    FStrikeThroughContent: string;
    FUnderlineTitle: string;
    FUnIndentContent: string;
    FInsertSpecialCharContent: string;
    FSubScriptTitle: string;
    FInsertPictureContent: string;
    FAlignRightContent: string;
    FSuperScriptContent: string;
    FTextColorContent: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property BoldTitle: string read FBoldTitle write FBoldTitle;
    property BoldContent: string read FBoldContent write FBoldContent;

    property ItalicTitle: string read FItalicTitle write FItalicTitle;
    property ItalicContent: string read FItalicContent write FItalicContent;

    property UnderlineTitle: string read FUnderlineTitle write FUnderlineTitle;
    property UnderlineContent: string read FUnderlineContent write FUnderlineContent;

    property StrikeThroughTitle: string read FStrikeThroughTitle write FStrikeThroughTitle;
    property StrikeThroughContent: string read FStrikeThroughContent write FStrikeThroughContent;

    property SubScriptTitle: string read FSubScriptTitle write FSubScriptTitle;
    property SubScriptContent: string read FSubScriptContent write FSubScriptContent;

    property SuperScriptTitle: string read FSuperScriptTitle write FSuperScriptTitle;
    property SuperScriptContent: string read FSuperScriptContent write FSuperScriptContent;

    property InsertPictureTitle: string read FInsertPictureTitle write FInsertPictureTitle;
    property InsertPictureContent: string read FInsertPictureContent write FInsertPictureContent;

    property InsertSpecialCharTitle: string read FInsertSpecialCharTitle write FInsertSpecialCharTitle;
    property InsertSpecialCharContent: string read FInsertSpecialCharContent write FInsertSpecialCharContent;

    property BulletTitle: string read FBulletTitle write FBulletTitle;
    property BulletContent: string read FBulletContent write FBulletContent;

    property NumberedBulletTitle: string read FNumberedBulletTitle write FNumberedBulletTitle;
    property NumberedBulletContent: string read FNumberedBulletContent write FNumberedBulletContent;

    property TextColorTitle: string read FTextColorTitle write FTextColorTitle;
    property TextColorContent: string read FTextColorContent write FTextColorContent;

    property BackgroundColorTitle: string read FBackgroundColorTitle write FBackgroundColorTitle;
    property BackgroundColorContent: string read FBackgroundColorContent write FBackgroundColorContent;

    property AlignLeftTitle: string read FAlignLeftTitle write FAlignLeftTitle;
    property AlignLeftContent: string read FAlignLeftContent write FAlignLeftContent;

    property AlignCenterTitle: string read FAlignCenterTitle write FAlignCenterTitle;
    property AlignCenterContent: string read FAlignCenterContent write FAlignCenterContent;

    property AlignRightTitle: string read FAlignRightTitle write FAlignRightTitle;
    property AlignRightContent: string read FAlignRightContent write FAlignRightContent;

    property InsertHyperlinkTitle: string read FInsertHyperlinkTitle write FInsertHyperlinkTitle;
    property InsertHyperlinkContent: string read FInsertHyperlinkContent write FInsertHyperlinkContent;

    property IndentTitle: string read FIndentTitle write FIndentTitle;
    property IndentContent: string read FIndentContent write FIndentContent;

    property UnIndentTitle: string read FUnIndentTitle write FUnIndentTitle;
    property UnIndentContent: string read FUnIndentContent write FUnIndentContent;
  end;


  TRichEditFormatToolBar = class(TRichEditToolBar)
  private
    FOptions: TRichEditToolBarFormatButtons;
    FHints: TRichEditFormatHints;
    procedure SetHints(const Value: TRichEditFormatHints);
  protected
    procedure SetOptions(Value: TRichEditToolBarFormatButtons);
    procedure UpdateButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Hints: TRichEditFormatHints read FHints write SetHints;
    property Options: TRichEditToolBarFormatButtons read FOptions write SetOptions;
  end;

  TRichEditToolBarEditButton = (btFileOpen, btFileSave, btCopy, btPaste, btCut, btUndo);

  TRichEditToolBarEditButtons = set of TRichEditToolBarEditButton;

  TRichEditEditHints = class(TPersistent)
  private
    FPasteContent: string;
    FUndoTitle: string;
    FRedoTitle: string;
    FFileSaveContent: string;
    FCopyTitle: string;
    FFileOpenContent: string;
    FUndoContent: string;
    FRedoContent: string;
    FCopyContent: string;
    FCutTitle: string;
    FPasteTitle: string;
    FFileSaveTitle: string;
    FCutContent: string;
    FFileOpenTitle: string;

  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property FileOpenTitle: string read FFileOpenTitle write FFileOpenTitle;
    property FileOpenContent: string read FFileOpenContent write FFileOpenContent;

    property FileSaveTitle: string read FFileSaveTitle write FFileSaveTitle;
    property FileSaveContent: string read FFileSaveContent write FFileSaveContent;

    property CutTitle: string read FCutTitle write FCutTitle;
    property CutContent: string read FCutContent write FCutContent;

    property CopyTitle: string read FCopyTitle write FCopyTitle;
    property CopyContent: string read FCopyContent write FCopyContent;

    property PasteTitle: string read FPasteTitle write FPasteTitle;
    property PasteContent: string read FPasteContent write FPasteContent;

    property UndoTitle: string read FUndoTitle write FUndoTitle;
    property UndoContent: string read FUndoContent write FUndoContent;

    property RedoTitle: string read FRedoTitle write FRedoTitle;
    property RedoContent: string read FRedoContent write FRedoContent;
  end;


  TRichEditEditToolBar = class(TRichEditToolBar)
  private
    FHints: TRichEditEditHints;
    FOptions: TRichEditToolBarEditButtons;
    procedure SetHints(const Value: TRichEditEditHints);
  protected
    procedure OpenFile(Sender: TObject);
    procedure SaveFile(Sender: TObject);

    procedure SetOptions(Value: TRichEditToolBarEditButtons);
    procedure UpdateButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Hints: TRichEditEditHints read FHints write SetHints;
    property Options: TRichEditToolBarEditButtons read FOptions write SetOptions;
  end;

  // Ribbon toolbars

  TRichEditClipboardHints = class(TPersistent)
  private
    FPasteContent: string;
    FCopyTitle: string;
    FCopyContent: string;
    FCutTitle: string;
    FPasteTitle: string;
    FCutContent: string;
  public
    constructor Create;
  published
    property CutTitle: string read FCutTitle write FCutTitle;
    property CutContent: string read FCutContent write FCutContent;

    property CopyTitle: string read FCopyTitle write FCopyTitle;
    property CopyContent: string read FCopyContent write FCopyContent;

    property PasteTitle: string read FPasteTitle write FPasteTitle;
    property PasteContent: string read FPasteContent write FPasteContent;

  end;

  TRichEditClipboardRibbonToolBar = class(TRichEditToolBar)
  private
    FCut,FCopy,FPaste: TAdvGlowButton;
    FHints: TRichEditClipboardHints;
    procedure SetHints(const Value: TRichEditClipboardHints);
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoPositionControls default false;
    property Hints: TRichEditClipboardHints read FHints write SetHints;
  end;

  TRichEditFontHints = class(TPersistent)
  private
    FUnderlineContent: string;
    FSubScriptContent: string;
    FItalicTitle: string;
    FBackgroundColorTitle: string;
    FBoldTitle: string;
    FStrikeThroughTitle: string;
    FSuperScriptTitle: string;
    FItalicContent: string;
    FBackgroundColorContent: string;
    FTextColorTitle: string;
    FBoldContent: string;
    FStrikeThroughContent: string;
    FUnderlineTitle: string;
    FSubScriptTitle: string;
    FSuperScriptContent: string;
    FTextColorContent: string;

  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property BoldTitle: string read FBoldTitle write FBoldTitle;
    property BoldContent: string read FBoldContent write FBoldContent;

    property ItalicTitle: string read FItalicTitle write FItalicTitle;
    property ItalicContent: string read FItalicContent write FItalicContent;

    property UnderlineTitle: string read FUnderlineTitle write FUnderlineTitle;
    property UnderlineContent: string read FUnderlineContent write FUnderlineContent;

    property StrikeThroughTitle: string read FStrikeThroughTitle write FStrikeThroughTitle;
    property StrikeThroughContent: string read FStrikeThroughContent write FStrikeThroughContent;

    property SubScriptTitle: string read FSubScriptTitle write FSubScriptTitle;
    property SubScriptContent: string read FSubScriptContent write FSubScriptContent;

    property SuperScriptTitle: string read FSuperScriptTitle write FSuperScriptTitle;
    property SuperScriptContent: string read FSuperScriptContent write FSuperScriptContent;

    property TextColorTitle: string read FTextColorTitle write FTextColorTitle;
    property TextColorContent: string read FTextColorContent write FTextColorContent;

    property BackgroundColorTitle: string read FBackgroundColorTitle write FBackgroundColorTitle;
    property BackgroundColorContent: string read FBackgroundColorContent write FBackgroundColorContent;
  end;


  TRichEditFontRibbonToolBar = class(TRichEditToolBar)
  private
    FHints: TRichEditFontHints;
    procedure SetHints(const Value: TRichEditFontHints);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Hints: TRichEditFontHints read FHints write SetHints;
  end;

  TRichEditParagraphHints = class(TPersistent)
  private
    FNumberedBulletContent: string;
    FBulletTitle: string;
    FAlignCenterTitle: string;
    FAlignLeftTitle: string;
    FIndentTitle: string;
    FUnIndentTitle: string;
    FAlignRightTitle: string;
    FBulletContent: string;
    FAlignCenterContent: string;
    FAlignLeftContent: string;
    FNumberedBulletTitle: string;
    FIndentContent: string;
    FUnIndentContent: string;
    FAlignRightContent: string;

  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property BulletTitle: string read FBulletTitle write FBulletTitle;
    property BulletContent: string read FBulletContent write FBulletContent;

    property NumberedBulletTitle: string read FNumberedBulletTitle write FNumberedBulletTitle;
    property NumberedBulletContent: string read FNumberedBulletContent write FNumberedBulletContent;

    property AlignLeftTitle: string read FAlignLeftTitle write FAlignLeftTitle;
    property AlignLeftContent: string read FAlignLeftContent write FAlignLeftContent;

    property AlignCenterTitle: string read FAlignCenterTitle write FAlignCenterTitle;
    property AlignCenterContent: string read FAlignCenterContent write FAlignCenterContent;

    property AlignRightTitle: string read FAlignRightTitle write FAlignRightTitle;
    property AlignRightContent: string read FAlignRightContent write FAlignRightContent;

    property IndentTitle: string read FIndentTitle write FIndentTitle;
    property IndentContent: string read FIndentContent write FIndentContent;

    property UnIndentTitle: string read FUnIndentTitle write FUnIndentTitle;
    property UnIndentContent: string read FUnIndentContent write FUnIndentContent;
  end;


  TRichEditParagraphRibbonToolBar = class(TRichEditToolBar)
  private
    FHints: TRichEditParagraphHints;
    procedure SetHints(const Value: TRichEditParagraphHints);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Hints: TRichEditParagraphHints read FHints write SetHints;
  end;

implementation

uses
  ExtDlgs, Dialogs, PNGImage, JPEG, Math, Windows;

const
  BTNSIZE = 24;

{ TRichEditToolBar }

constructor TRichEditFormatToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
  sep: TAdvToolBarSeparator;
  fs: TAdvOfficeFontSelector;
  fss: TAdvOfficeFontSizeSelector;
  cs: TAdvOfficeColorSelector;
  actn: TAction;
begin
  inherited;

  FHints := TRichEditFormatHints.Create;

  ShowRightHandle := false;

  fs := TAdvOfficeFontSelector.Create(Self);
  fs.OnSelectFontName := SelectFontName;
  fs.OnExit := ExitFontName;
  actn := TRichEditFontNameAction.Create(Self);
  actn.ActionComponent := fs;
  fs.Action := actn;
  AddToolBarControl(fs);

  fss := TAdvOfficeFontSizeSelector.Create(Self);
  fss.OnSelectFontSize := SelectFontSize;
  fss.OnExit := ExitFontSize;
  actn := TRichEditFontSizeAction.Create(Self);
  actn.ActionComponent := fss;
  fss.Action := actn;
  AddToolBarControl(fss);

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

  atb := AddButton(HInstance,TRichEditBold, bsCheck, 'TMSRETBBOLD','Bold');
  atb.OfficeHint.Title := Hints.BoldTitle;
  atb.OfficeHint.Notes.Text := Hints.BoldContent;
  atb.Tag := integer(btBold);

  atb := AddButton(HInstance,TRichEditItalic, bsCheck, 'TMSRETBITALIC','Italic');
  atb.OfficeHint.Title := Hints.ItalicTitle;
  atb.OfficeHint.Notes.Text := Hints.ItalicContent;
  atb.Tag := integer(btItalic);

  atb := AddButton(HInstance,TRichEditUnderline, bsCheck, 'TMSRETBUNDERLINE','Underline');
  atb.OfficeHint.Title := Hints.UnderlineTitle;
  atb.OfficeHint.Notes.Text := Hints.UnderlineContent;
  atb.Tag := integer(btUnderline);

  atb := AddButton(HInstance,TRichEditStrikeOut, bsCheck, 'TMSRETBSTRIKE','Strikethrough');
  atb.OfficeHint.Title := Hints.StrikeThroughTitle;
  atb.OfficeHint.Notes.Text := Hints.StrikeThroughContent;
  atb.Tag := integer(btStrikeThrough);

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

  atb := AddButton(HInstance,TRichEditAlignLeft, bsCheck, 'TMSRETBALIGNLEFT','Align left');
  atb.OfficeHint.Title := Hints.AlignLeftTitle;
  atb.OfficeHint.Notes.Text := Hints.AlignLeftContent;
  atb.Tag := integer(btAlignLeft);

  atb := AddButton(HInstance,TRichEditAlignCenter, bscheck, 'TMSRETBALIGNCENTER','Align center');
  atb.OfficeHint.Title := Hints.AlignCenterTitle;
  atb.OfficeHint.Notes.Text := Hints.AlignCenterContent;
  atb.Tag := integer(btAlignCenter);

  atb := AddButton(HInstance,TRichEditAlignRight, bsCheck, 'TMSRETBALIGNRIGHT','Align right');
  atb.OfficeHint.Title := Hints.AlignRightTitle;
  atb.OfficeHint.Notes.Text := Hints.AlignRightContent;
  atb.Tag := integer(btAlignRight);

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

  cs := TAdvOfficeColorSelector.Create(Self);
  cs.SelectedColor := clBlack;
  cs.ShowDisabled := false;
  cs.OnSelectColor := SelectTextColor;
  cs.Tools.Items[0].BackGroundColor := Font.Color;
  cs.Tag := integer(btTextColor);
  cs.OfficeHint.Title := Hints.TextColorTitle;
  cs.OfficeHint.Notes.Text := Hints.TextColorContent;
  cs.Width := atb.Width;
  cs.Height := atb.Height;

  actn := TRichEditTextColorAction.Create(Self);
  actn.ActionComponent := cs;

  cs.Action := actn;

  AddToolBarControl(cs);

  cs := TAdvOfficeColorSelector.Create(Self);
  cs.OnSelectColor := SelectColor;
  cs.SelectedColor := clWhite;
  cs.ShowDisabled := false;
  cs.Tools.Items[0].BackGroundColor := Color;
  cs.Tag := integer(btBackgroundColor);
  cs.OfficeHint.Title := Hints.BackgroundColorTitle;
  cs.OfficeHint.Notes.Text := Hints.BackgroundColorContent;
  cs.Width := atb.Width;
  cs.Height := atb.Height;

  actn := TRichEditColorAction.Create(Self);
  actn.ActionComponent := cs;
  cs.Action := actn;

  AddToolBarControl(cs);

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

  atb := AddButton(HInstance,TRichEditBullets, bsCheck, 'TMSRETBOL','Numbered');
  atb.OfficeHint.Title := Hints.NumberedBulletTitle;
  atb.OfficeHint.Notes.Text := Hints.NumberedBulletContent;
  atb.Tag := integer(btBullet);

  Options := [btBold, btItalic, btUnderline, btStrikeThrough,
    btBullet, btTextColor, btBackgroundColor, btAlignLeft, btAlignCenter,
    btAlignRight ];
end;


destructor TRichEditFormatToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;

{ TRichEditToolBar }

procedure TRichEditToolBar.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = RichEdit) then
    FRichEdit := nil;
end;

procedure TRichEditToolBar.SelectBullet(Sender: TObject;
  Index: Integer; Item: TAdvSelectorItem);
begin
  if Assigned(RichEdit) then
  begin
//    RichEdit.SetSelectionBullets(TBulletType(Index));
//    RichEdit.SetFocus;
  end;
end;

procedure TRichEditToolBar.SelectionBullets(Sender: TObject);
begin
  if Assigned(RichEdit) then
  begin
//    RichEdit.SetSelectionBullets(TBulletType((Sender as TAdvOfficeToolSelector).SelectedIndex));
//    RichEdit.SetFocus;
  end;
end;

procedure TRichEditToolBar.SelectionNumbering(Sender: TObject);
begin
  if Assigned(RichEdit) then
  begin
//    RichEdit.SetSelectionBullets(btNumber);
//    RichEdit.SetFocus;
  end;
end;

procedure TRichEditToolBar.SelectColor(Sender: TObject;
  AColor: TColor);
var
  Format: CHARFORMAT2;
begin
  if Assigned(RichEdit) then
  begin

    FillChar(Format, SizeOf(Format), 0);
    with Format do
    begin
      cbSize := SizeOf(Format);
      dwMask := CFM_BACKCOLOR;
      crBackColor := AColor;
      Richedit.Perform(EM_SETCHARFORMAT, SCF_SELECTION, Longint(@Format));
    end;
  end;
end;

procedure TRichEditToolBar.SelectFontName(Sender: TObject;
  AName: string);
begin
  if Assigned(RichEdit) then
  begin
    RichEdit.SelAttributes.Name := AName;
    RichEdit.SetFocus;
  end;
end;

procedure TRichEditToolBar.SelectFontSize(Sender: TObject;
  ASize: integer);
begin
  if Assigned(RichEdit) then
  begin
    RichEdit.SelAttributes.Size := ASize;
    RichEdit.SetFocus;
  end;
end;

procedure TRichEditToolBar.SelectTextColor(Sender: TObject;
  AColor: TColor);
begin
  if Assigned(RichEdit) then
  begin
    RichEdit.SelAttributes.Color := AColor;
    RichEdit.SetFocus;
  end;
end;

procedure TRichEditToolBar.ExitFontName(Sender: TObject);
var
  fn: string;
  i: integer;
begin
  if not Assigned(RichEdit) then
    Exit;

  if (Sender as TAdvOfficeFontSelector).DroppedDown then
    Exit;

  fn := (Sender as TAdvOfficeFontSelector).Text;

  i := (Sender as TAdvOfficeFontSelector).Items.IndexOf(fn);

  if (i >= 0) then
  begin
    (Sender as TAdvOfficeFontSelector).SelectedFontName := fn;
    RichEdit.SelAttributes.Name := fn;
    RichEdit.SetFocus;
  end;
end;

procedure TRichEditToolBar.ExitFontSize(Sender: TObject);
var
  fs,e: integer;

begin
  if not Assigned(RichEdit) then
    Exit;

  if (Sender as TAdvOfficeFontSizeSelector).DroppedDown then
    Exit;

  val((Sender as TAdvOfficeFontSizeSelector).Text,fs,e);

  if (e = 0) and (fs > 0) then
  begin
    (Sender as TAdvOfficeFontSizeSelector).SelectedFontSize := fs;
    RichEdit.SelAttributes.Size := fs;
    RichEdit.SetFocus;
  end;
end;


{ TRichEditEditToolBar }

constructor TRichEditEditToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
  sep: TAdvToolBarSeparator;
begin
  inherited;

  FHints := TRichEditEditHints.Create;

  ShowRightHandle := false;

  atb := AddButton(HInstance,nil, bsButton, 'TMSRETBOPEN','Open file');
  atb.OfficeHint.Title := FHints.FileOpenTitle;
  atb.OfficeHint.Notes.Text := FHints.FileOpenContent;
  atb.OnClick := OpenFile;
  atb.Tag := integer(btFileOpen);

  atb := AddButton(HInstance,nil, bsButton, 'TMSRETBSAVE','Save');
  atb.OfficeHint.Title := FHints.FileSaveTitle;
  atb.OfficeHint.Notes.Text := FHints.FileSaveContent;
  atb.OnClick := SaveFile;
  atb.Tag := integer(btFileSave);

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

  atb := AddButton(HInstance,TEditCut, bsButton, 'TMSRETBCUT', 'Cut to clipboard');
  atb.OfficeHint.Title := FHints.CutTitle;
  atb.OfficeHint.Notes.Text := FHints.CutContent;
  atb.Tag := integer(btCut);

  atb := AddButton(HInstance,TEditCopy, bsButton, 'TMSRETBCOPY', 'Copy to clipboard');
  atb.OfficeHint.Title := FHints.CopyTitle;
  atb.OfficeHint.Notes.Text := FHints.CopyContent;
  atb.Tag := integer(btCopy);

  atb := AddButton(HInstance,TEditPaste, bsButton, 'TMSRETBPASTE', 'Paste from clipboard');
  atb.OfficeHint.Title := FHints.PasteTitle;
  atb.OfficeHint.Notes.Text := FHints.PasteContent;
  atb.Tag := integer(btPaste);

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

  atb := AddButton(HInstance,TEditUndo, bsButton, 'TMSRETBUNDO', 'Undo');
  atb.OfficeHint.Title := FHints.UndoTitle;
  atb.OfficeHint.Notes.Text := FHints.UndoContent;
  atb.Tag := integer(btUndo);

  Options := [btFileOpen, btFileSave, btCopy, btPaste, btCut, btUndo];
end;

destructor TRichEditEditToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;

procedure TRichEditEditToolBar.OpenFile(Sender: TObject);
var
  od: TOpenDialog;
  fe: string;
begin
  od := TOpenDialog.Create(Self);
  od.Filter := 'Text files|*.txt|RTF files|*.rtf|All files|*.*';
  try

    if od.Execute then
      if Assigned(FRichEdit) then
      begin
        fe := Uppercase(ExtractFileExt(od.FileName));
        FRichEdit.PlainText := fe = '.TXT';
        FRichEdit.Lines.LoadFromFile(od.FileName);
      end;

  finally
    od.Free;
  end;
end;

procedure TRichEditEditToolBar.SaveFile(Sender: TObject);
var
  sd: TSaveDialog;
  fe,fn: string;
begin
  sd := TSaveDIalog.Create(Self);
  sd.Filter := 'Text files|*.txt|RTF files|*.rtf';
  try
    if sd.Execute then
    begin
      fn := sd.FileName;
      fe := Uppercase(ExtractFileExt(sd.FileName));

      if (fe = '') then
      begin
        if sd.FilterIndex = 1 then
          fn := fn + '.txt';
        if sd.FilterIndex = 2 then
          fn := fn + '.rtf';
      end;

      FRichEdit.PlainText := sd.FilterIndex = 1;
      FRichEdit.Lines.SaveToFile(fn);
    end;
  finally
    sd.Free;
  end;
end;

procedure TRichEditFormatToolBar.SetHints(
  const Value: TRichEditFormatHints);
begin
  FHints.Assign(Value);
end;

procedure TRichEditFormatToolBar.SetOptions(
  Value: TRichEditToolBarFormatButtons);
begin
  FOptions := Value;
  UpdateButtons;
end;

procedure TRichEditFormatToolBar.UpdateButtons;
var
  i: integer;
  j: TRichEditToolBarFormatButton;
begin
  for i := 0 to ControlCount - 1 do
  begin
    for j := Low(TRichEditToolBarFormatButton) to High(TRichEditToolBarFormatButton) do
    begin
      if Controls[i].Tag = integer(j) then
        Controls[i].Visible := j in Options;
    end;
  end;
end;

procedure TRichEditEditToolBar.SetHints(
  const Value: TRichEditEditHints);
begin
  FHints.Assign(Value);
end;

procedure TRichEditEditToolBar.SetOptions(
  Value: TRichEditToolBarEditButtons);
begin
  FOptions := Value;
  UpdateButtons;
end;

procedure TRichEditEditToolBar.UpdateButtons;
var
  i: integer;
  j: TRichEditToolBarEditButton;
begin
  for i := 0 to ControlCount - 1 do
  begin
    for j := Low(TRichEditToolBarEditButton) to High(TRichEditToolBarEditButton) do
    begin
      if Controls[i].Tag = integer(j) then
        Controls[i].Visible := j in Options;
    end;
  end;
end;



{ TRichEditClipboardRibbonToolBar }

constructor TRichEditClipboardRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
begin
  inherited;

  FHints := TRichEditClipboardHints.Create;

  Caption := 'Clipboard';
  CaptionAlignment := taCenter;
  CaptionPosition := cpBottom;
  ShowRightHandle := false;
  ShowCaption := true;
  ShowOptionIndicator := false;

  ToolBarState := tsFixed;
  AutoPositionControls := false;
  AutoSize := false;

  atb := AddButton(HInstance, TEditPaste, bsButton, 'TMSRETBPASTELARGE', 'Paste from clipboard');
  atb.Tag := 1;
  atb.OfficeHint.Title := FHints.PasteTitle;
  atb.OfficeHint.Notes.Text := FHints.PasteContent;
  atb.Caption := 'Paste';
  atb.ShowCaption := true;
  atb.DropDownButton := true;
  atb.DropDownPosition := dpBottom;
  atb.MinButtonSizeState := bsLarge;
  atb.MaxButtonSizeState := bsLarge;
  atb.Layout := blGlyphTop;
  atb.Width := 40;
  atb.Height := 64;

  FPaste := atb;

  atb := AddButton(HInstance, TEditCut, bsButton, 'TMSRETBCUT', 'Cut to clipboard');
  atb.Caption := 'Cut';
  atb.OfficeHint.Title := FHints.CutTitle;
  atb.OfficeHint.Notes.Text := FHints.CutContent;
  atb.ShowCaption := true;
  atb.Tag := 2;
  atb.Width := 60;
  atb.Height := 24;
  atb.Left := 41;
  atb.Top := 2;

  FCut := atb;

  atb := AddButton(HInstance, TEditCopy, bsButton, 'TMSRETBCOPY', 'Copy to clipboard');
  atb.OfficeHint.Title := FHints.CopyTitle;
  atb.OfficeHint.Notes.Text := FHints.CopyContent;
  atb.Caption := 'Copy';
  atb.ShowCaption := true;
  atb.Tag := 3;
  atb.Width := 60;
  atb.Height := 24;
  atb.Left := 41;
  atb.Top := 26;

  FCopy := atb;

  Width := 102;
  Height := 85;
end;

procedure TRichEditClipboardRibbonToolBar.CreateWnd;
begin
  inherited;
  AutoPositionControls := false;
  ToolBarState := tsFixed;
  FCopy.Top := 24;
  FCopy.Left := 41;
end;

destructor TRichEditClipboardRibbonToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;

procedure TRichEditClipboardRibbonToolBar.SetHints(
  const Value: TRichEditClipboardHints);
begin
  FHints := Value;
end;

{ TRichEditFontRibbonToolBar }

constructor TRichEditFontRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
  fs: TAdvOfficeFontSelector;
  fss: TAdvOfficeFontSizeSelector;
  cs: TAdvOfficeColorSelector;
  actn: TAction;
begin
  inherited;

  FHints := TRichEditFontHints.Create;

  Caption:= 'Font';
  CaptionAlignment := taCenter;
  CaptionPosition := cpBottom;
  ShowRightHandle := false;
  ShowCaption := true;
  ShowOptionIndicator := false;

  ToolBarState := tsFixed;
  AutoPositionControls := false;
  AutoSize := false;

  fs := TAdvOfficeFontSelector.Create(Self);
  fs.OnSelectFontName := SelectFontName;
 // fs.OnExit := ExitFontName;
  actn := TRichEditFontNameAction.Create(Self);
  actn.ActionComponent := fs;
  fs.Action := actn;

  AddToolBarControl(fs);
  fs.Left := 2;
  fs.Top := 2;

  fss := TAdvOfficeFontSizeSelector.Create(Self);
  fss.OnSelectFontSize := SelectFontSize;
//  fss.OnExit := ExitFontSize;

  actn := TRichEditFontSizeAction.Create(Self);
  actn.ActionComponent := fss;
  fss.Action := actn;

  AddToolBarControl(fss);
  fss.Left := fs.Left + fs.Width;
  fss.Top := 2;


  atb := AddButton(HInstance, TRichEditBold, bsButton, 'TMSRETBBOLD', 'Bold');
  atb.OfficeHint.Title := FHints.BoldTitle;
  atb.OfficeHint.Notes.Text := FHints.BoldContent;
  atb.Left := 2;
  atb.Top := 26;
  atb.Position := bpLeft;

  atb := AddButton(HInstance, TRichEditItalic, bsButton, 'TMSRETBITALIC', 'Italic');
  atb.OfficeHint.Title := FHints.ItalicTitle;
  atb.OfficeHint.Notes.Text := FHints.ItalicContent;
  atb.Left := 26;
  atb.Top := 26;
  atb.Position := bpMiddle;

  atb := AddButton(HInstance, TRichEditUnderline, bsButton, 'TMSRETBUNDERLINE', 'Underline');
  atb.OfficeHint.Title := FHints.UnderlineTitle;
  atb.OfficeHint.Notes.Text := FHints.UnderlineContent;
  atb.Left := 50;
  atb.Top := 26;
  atb.Position := bpMiddle;

  atb := AddButton(HInstance, TRichEditStrikeout, bsButton, 'TMSRETBSTRIKE', 'Strikeout');
  atb.OfficeHint.Title := FHints.StrikeThroughTitle;
  atb.OfficeHint.Notes.Text := FHints.StrikeThroughContent;
  atb.Left := 74;
  atb.Top := 26;
  atb.Position := bpMiddle;

  cs := TAdvOfficeColorSelector.Create(Self);
  cs.OfficeHint.Title := FHints.TextColorTitle;
  cs.OfficeHint.Notes.Text := FHints.TextColorContent;
  cs.SelectedColor := clBlack;
  cs.ShowDisabled := false;
  cs.OnSelectColor := SelectTextColor;
  cs.Tools.Items[0].BackGroundColor := Font.Color;
  cs.Tag := integer(btTextColor);
  cs.OfficeHint.Title := 'Text color';
  cs.OfficeHint.Notes.Text := 'Set selection text color';
  cs.Top := 26;
  cs.Left := 150;
  cs.Width := 24;
  cs.Height := 24;
  cs.Position := bpLeft;

  actn := TRichEditTextColorAction.Create(Self);
  actn.ActionComponent := cs;
  cs.Action := actn;

  AddToolBarControl(cs);

  cs := TAdvOfficeColorSelector.Create(Self);
  cs.OfficeHint.Title := FHints.BackgroundColorTitle;
  cs.OfficeHint.Notes.Text := FHints.BackgroundColorContent;
  cs.OnSelectColor := SelectColor;
  cs.SelectedColor := clWhite;
  cs.ShowDisabled := false;
  cs.Tools.Items[0].BackGroundColor := Color;
  cs.Tag := integer(btBackgroundColor);
  cs.Top := 26;
  cs.Left := 174;
  cs.Width := 24;
  cs.Height := 24;
  cs.Position := bpRight;

  actn := TRichEditColorAction.Create(Self);
  actn.ActionComponent := cs;
  cs.Action := actn;


  AddToolBarControl(cs);

  Width := 205;
  Height := 85;
end;

destructor TRichEditFontRibbonToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;

procedure TRichEditFontRibbonToolBar.SetHints(
  const Value: TRichEditFontHints);
begin
  FHints.Assign(Value);
end;

{ TRichEditParagraphRibbonToolBar }

constructor TRichEditParagraphRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
begin
  inherited;

  FHints := TRichEditParagraphHints.Create;

  Caption := 'Paragraph';
  CaptionAlignment := taCenter;
  CaptionPosition := cpBottom;
  ShowRightHandle := false;
  ShowCaption := true;
  ShowOptionIndicator := false;

  ToolBarState := tsFixed;
  AutoPositionControls := false;
  AutoSize := false;

  atb := AddButton(HInstance,TRichEditAlignLeft, bsCheck, 'TMSRETBALIGNLEFT','Align left');
  atb.OfficeHint.Title := FHints.AlignLeftTitle;
  atb.OfficeHint.Notes.Text := FHints.AlignLeftContent;
  atb.Tag := integer(btAlignLeft);
  atb.Left := 2;
  atb.Top := 26;
  atb.Position := bpLeft;

  atb := AddButton(HInstance,TRichEditAlignCenter, bscheck, 'TMSRETBALIGNCENTER','Align center');
  atb.OfficeHint.Title := FHints.AlignCenterTitle;
  atb.OfficeHint.Notes.Text := FHints.AlignCenterContent;
  atb.Tag := integer(btAlignCenter);
  atb.Left := 26;
  atb.Top := 26;
  atb.Position := bpMiddle;

  atb := AddButton(HInstance,TRichEditAlignRight, bsCheck, 'TMSRETBALIGNRIGHT','Align right');
  atb.OfficeHint.Title := FHints.AlignRightTitle;
  atb.OfficeHint.Notes.Text := FHints.AlignRightContent;
  atb.Left := 50;
  atb.Top := 26;
  atb.Tag := integer(btAlignRight);
  atb.Position := bpRight;

  atb := AddButton(HInstance,TRichEditBullets, bsCheck, 'TMSRETBOL','Numbered');
  atb.OfficeHint.Title := FHints.NumberedBulletTitle;
  atb.OfficeHint.Notes.Text := FHints.NumberedBulletContent;
  atb.Tag := integer(btBullet);
  atb.Left := 36;
  atb.Top := 2;

  Width := 110;
  Height := 85;
end;


destructor TRichEditParagraphRibbonToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;


procedure TRichEditParagraphRibbonToolBar.SetHints(
  const Value: TRichEditParagraphHints);
begin
  FHints.Assign(Value);
end;

{ TRichEditFormatHints }

procedure TRichEditFormatHints.Assign(Source: TPersistent);
begin
  if (Source is TRichEditFormatHints) then
  begin
    FNumberedBulletContent := (Source as TRichEditFormatHints).NumberedBulletContent;
    FUnderlineContent := (Source as TRichEditFormatHints).UnderlineContent;
    FSubScriptContent := (Source as TRichEditFormatHints).SubScriptContent;
    FItalicTitle := (Source as TRichEditFormatHints).ItalicTitle;
    FInsertHyperlinkTitle := (Source as TRichEditFormatHints).InsertHyperlinkTitle;
    FBackgroundColorTitle := (Source as TRichEditFormatHints).BackgroundColorTitle;
    FBulletTitle := (Source as TRichEditFormatHints).BulletTitle;
    FAlignCenterTitle := (Source as TRichEditFormatHints).AlignCenterTitle;
    FAlignLeftTitle := (Source as TRichEditFormatHints).AlignLeftTitle;
    FBoldTitle := (Source as TRichEditFormatHints).BoldTitle;
    FIndentTitle := (Source as TRichEditFormatHints).IndentTitle;
    FStrikeThroughTitle := (Source as TRichEditFormatHints).StrikeThroughTitle;
    FUnIndentTitle := (Source as TRichEditFormatHints).UnIndentTitle;
    FInsertSpecialCharTitle := (Source as TRichEditFormatHints).InsertSpecialCharTitle;
    FInsertPictureTitle := (Source as TRichEditFormatHints).InsertSpecialCharTitle;
    FAlignRightTitle := (Source as TRichEditFormatHints).AlignRightTitle;
    FSuperScriptTitle := (Source as TRichEditFormatHints).SuperScriptTitle;
    FItalicContent := (Source as TRichEditFormatHints).ItalicContent;
    FInsertHyperlinkContent := (Source as TRichEditFormatHints).InsertHyperlinkContent;
    FBackgroundColorContent := (Source as TRichEditFormatHints).BackgroundColorContent;
    FTextColorTitle := (Source as TRichEditFormatHints).TextColorTitle;
    FBulletContent := (Source as TRichEditFormatHints).BulletContent;
    FAlignCenterContent := (Source as TRichEditFormatHints).AlignCenterContent;
    FAlignLeftContent := (Source as TRichEditFormatHints).AlignLeftContent;
    FNumberedBulletTitle := (Source as TRichEditFormatHints).NumberedBulletTitle;
    FBoldContent := (Source as TRichEditFormatHints).BoldContent;
    FIndentContent := (Source as TRichEditFormatHints).IndentContent;
    FStrikeThroughContent := (Source as TRichEditFormatHints).StrikeThroughContent;
    FUnderlineTitle := (Source as TRichEditFormatHints).UnderlineTitle;
    FUnIndentContent := (Source as TRichEditFormatHints).UnIndentContent;
    FInsertSpecialCharContent := (Source as TRichEditFormatHints).InsertSpecialCharContent;
    FSubScriptTitle := (Source as TRichEditFormatHints).SubScriptTitle;
    FInsertPictureContent := (Source as TRichEditFormatHints).InsertPictureContent;
    FAlignRightContent := (Source as TRichEditFormatHints).AlignRightContent;
    FSuperScriptContent := (Source as TRichEditFormatHints).SuperScriptContent;
    FTextColorContent := (Source as TRichEditFormatHints).TextColorContent;
  end;
end;

constructor TRichEditFormatHints.Create;
begin
  inherited;

  FItalicTitle := 'Italic (Ctrl+I)';
  FItalicContent := 'Select italic font style';

  FBoldTitle := 'Bold (Ctrl+B)';
  FBoldContent := 'Select bold font style';

  FStrikeThroughTitle := 'Strikethrough';
  FUnderlineTitle := 'Underline (Ctrl+U)';

  FUnderlineContent := 'Select underline font style';
  FStrikeThroughContent := 'Select strikethrough font style';

  FSuperScriptTitle := 'Superscript';
  FSuperScriptContent := 'Set superscript text';

  FSubScriptTitle := 'Subscript';
  FSubScriptContent := 'Set subscript text';

  FAlignLeftTitle := 'Align text left (Ctrl+L)';
  FAlignLeftContent := 'Align the text to left';

  FAlignCenterTitle := 'Align center (Ctrl+E)';
  FAlignCenterContent := 'Center text';

  FAlignRightTitle := 'Align text right (Ctrl+R)';
  FAlignRightContent := 'Align the text to right';

  FBulletTitle := 'Insert bullet';
  FBulletContent := 'Insert a bullet for list';

  FNumberedBulletTitle := 'Start list';
  FNumberedBulletContent := 'Start a numbered list';

  FIndentTitle := 'Increase indent';
  FIndentContent := 'Increase the indent level of the paragraph';

  FUnIndentTitle := 'Decrease indent';
  FUnIndentContent := 'Decrease the indent level of the paragraph';

  FInsertPictureTitle := 'Insert picture';
  FInsertPictureContent := 'Insert a picture from file';

  FBackgroundColorTitle := 'Background color';
  FBackgroundColorContent := 'Set selection background color';

  FTextColorTitle := 'Text color';
  FTextColorContent := 'Set selection text color';

  FInsertHyperlinkTitle := 'Set hyperlink';
  FInsertHyperlinkContent := 'Set hyperlink for text';

  FInsertSpecialCharTitle := 'Insert special character';
  FInsertSpecialCharContent := 'Insert a special character';
end;

{ TRichEditEditHints }

procedure TRichEditEditHints.Assign(Source: TPersistent);
begin
  if (Source is TRichEditEditHints) then
  begin
    FPasteContent := (Source as TRichEditEditHints).PasteContent;
    FUndoTitle := (Source as TRichEditEditHints).UndoTitle;
    FRedoTitle := (Source as TRichEditEditHints).RedoTitle;
    FFileSaveContent := (Source as TRichEditEditHints).FileSaveContent;
    FCopyTitle := (Source as TRichEditEditHints).CopyTitle;
    FFileOpenContent := (Source as TRichEditEditHints).FileOpenContent;
    FUndoContent := (Source as TRichEditEditHints).UndoContent;
    FRedoContent := (Source as TRichEditEditHints).RedoContent;
    FCopyContent := (Source as TRichEditEditHints).CopyContent;
    FCutTitle := (Source as TRichEditEditHints).CutTitle;
    FPasteTitle := (Source as TRichEditEditHints).PasteTitle;
    FFileSaveTitle := (Source as TRichEditEditHints).FileSaveTitle;
    FCutContent := (Source as TRichEditEditHints).CutContent;
    FFileOpenTitle := (Source as TRichEditEditHints).FileOpenTitle;
  end;

end;

constructor TRichEditEditHints.Create;
begin
  inherited;

  FPasteTitle := 'Paste (Ctrl+V)';
  FPasteContent := 'Add content on the clipboard to your document';

  FCopyTitle := 'Copy (Ctrl+C)';
  FCopyContent := 'Put a copy of the selection on the clipboard';

  FCutTitle := 'Cut (Ctrl+X)';
  FCutContent := 'Remove the selection to the clipboard';

  FUndoTitle := 'Undo (Ctrl+Z)';
  FUndoContent := 'Undo typing';

  FRedoTitle := 'Redo (Ctrl+Y)';
  FRedoContent := 'Redo typing';

  FFileSaveTitle := 'Save (Ctrl+S)';
  FFileSaveContent := 'Save document to file';

  FFileOpenTitle := 'Open (Ctrl+O)';
  FFileOpenContent := 'Open new document from file';
end;

{ TRichEditParagraphHints }

procedure TRichEditParagraphHints.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TRichEditParagraphHints.Create;
begin
  inherited;

  FAlignLeftTitle := 'Align text left (Ctrl+L)';
  FAlignLeftContent := 'Align the text to left';

  FAlignCenterTitle := 'Align center (Ctrl+E)';
  FAlignCenterContent := 'Center text';

  FAlignRightTitle := 'Align text right (Ctrl+R)';
  FAlignRightContent := 'Align the text to right';

  FBulletTitle := 'Insert bullet';
  FBulletContent := 'Insert a bullet for list';

  FNumberedBulletTitle := 'Start list';
  FNumberedBulletContent := 'Start a numbered list';

  FIndentTitle := 'Increase indent';
  FIndentContent := 'Increase the indent level of the paragraph';

  FUnIndentTitle := 'Decrease indent';
  FUnIndentContent := 'Decrease the indent level of the paragraph';

end;

{ TRichEditFontHints }

procedure TRichEditFontHints.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TRichEditFontHints.Create;
begin
  inherited Create;

  FItalicTitle := 'Italic (Ctrl+I)';
  FItalicContent := 'Select italic font style';

  FBoldTitle := 'Bold (Ctrl+B)';
  FBoldContent := 'Select bold font style';

  FStrikeThroughTitle := 'Strikethrough';
  FUnderlineTitle := 'Underline (Ctrl+U)';

  FUnderlineContent := 'Select underline font style';
  FStrikeThroughContent := 'Select strikethrough font style';

  FSuperScriptTitle := 'Superscript';
  FSuperScriptContent := 'Set superscript text';

  FSubScriptTitle := 'Subscript';
  FSubScriptContent := 'Set subscript text';

  FBackgroundColorTitle := 'Background color';
  FBackgroundColorContent := 'Set selection background color';

  FTextColorTitle := 'Text color';
  FTextColorContent := 'Set selection text color';

end;

{ TRichEditClipboardHints }

constructor TRichEditClipboardHints.Create;
begin
  inherited;

  FPasteTitle := 'Paste (Ctrl+V)';
  FPasteContent := 'Add content on the clipboard to your document';

  FCopyTitle := 'Copy (Ctrl+C)';
  FCopyContent := 'Put a copy of the selection on the clipboard';

  FCutTitle := 'Cut (Ctrl+X)';
  FCutContent := 'Remove the selection to the clipboard';
end;

{ TRichEditTextColorAction }

function TRichEditTextColorAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := Assigned(Target) and (Target is TRichEdit);
end;

procedure TRichEditTextColorAction.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := Assigned(Target) and (Target is TRichEdit) and (Target as TRichEdit).Focused;

  // get selection color here and update
  if Enabled then
  begin
    (ActionComponent as TAdvOfficeColorSelector).SelectedColor := (Target as TRichEdit).SelAttributes.Color;
  end;
end;

{ TRichEditColorAction }

function TRichEditColorAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := Assigned(Target) and (Target is TRichEdit);
end;

procedure TRichEditColorAction.UpdateTarget(Target: TObject);
var
  Format: CHARFORMAT2;

begin
  inherited;
  Enabled := Assigned(Target) and (Target is TRichEdit) and (Target as TRichEdit).Focused;

  if Enabled then
  begin
    FillChar(Format, SizeOf(Format), 0);
    with Format do
    begin
      cbSize := SizeOf(Format);
      (Target as TRichedit).Perform(EM_GETCHARFORMAT, SCF_SELECTION, Longint(@Format));

      if dwEffects and CFE_AUTOBACKCOLOR = CFE_AUTOBACKCOLOR then
        (ActionComponent as TAdvOfficeColorSelector).SelectedColor := (Target as TRichEdit).Color
      else
        (ActionComponent as TAdvOfficeColorSelector).SelectedColor := crBackColor;
    end;
  end;
end;



{ TRichEditFontNameAction }

function TRichEditFontNameAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := Assigned(Target) and (Target is TRichEdit);
end;

procedure TRichEditFontNameAction.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := Assigned(Target) and (Target is TRichEdit);

  // get selection color here and update
  if Enabled then
  begin
    (ActionComponent as TAdvOfficeFontSelector).SelectedFontName := (Target as TRichEdit).SelAttributes.Name;
  end;

end;

{ TRichEditFontSizeAction }

function TRichEditFontSizeAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := Assigned(Target) and (Target is TRichEdit);
end;

procedure TRichEditFontSizeAction.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := Assigned(Target) and (Target is TRichEdit);

  // get selection color here and update
  if Enabled then
  begin
    (ActionComponent as TAdvOfficeFontSizeSelector).SelectedFontSize := (Target as TRichEdit).SelAttributes.Size;
  end;
end;




end.
