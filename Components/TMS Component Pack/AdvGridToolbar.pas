{*************************************************************************}
{ TMS TAdvStringGrid toolbars                                             }
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

unit AdvGridToolbar;

interface

uses
  Classes, AdvToolBar, AdvToolBarExt, AdvToolBarRes, Graphics, AdvGrid, AdvGridActns,
  ActnList, AdvOfficeSelectors, AdvOfficeComboBox, AdvGlowButton, SysUtils;

type

  TAdvStringGridToolBar = class(TCustomAdvToolBar)
  private
    FGrid: TAdvStringGrid;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure SelectFontName(Sender: TObject; AName: string);
    procedure SelectFontSize(Sender: TObject; ASize: integer);
    procedure SelectColor(Sender: TObject; AColor: TColor);
    procedure SelectTextColor(Sender: TObject; AColor: TColor);
    procedure ExitFontSize(Sender: TObject);
    procedure ExitFontName(Sender: TObject);
  public
    function GetButton(Id: integer): TAdvGlowButton;
  published
    property Grid: TAdvStringGrid read FGrid write FGrid;
  end;

  TAdvStringGridToolBarFormatButton = (btFontName, btFontSize, btBold, btItalic, btUnderline, btStrikeThrough,
    btTextColor, btBackgroundColor, btAlignLeft, btAlignCenter, btAlignRight);

  TAdvStringGridToolBarFormatButtons = set of TAdvStringGridToolBarFormatButton;

  TAdvStringGridFormatHints = class(TPersistent)
  private
    FUnderlineContent: string;
    FItalicTitle: string;
    FBackgroundColorTitle: string;
    FAlignCenterTitle: string;
    FAlignLeftTitle: string;
    FBoldTitle: string;
    FStrikeThroughTitle: string;
    FAlignRightTitle: string;
    FItalicContent: string;
    FBackgroundColorContent: string;
    FTextColorTitle: string;
    FAlignCenterContent: string;
    FAlignLeftContent: string;
    FBoldContent: string;
    FStrikeThroughContent: string;
    FUnderlineTitle: string;
    FAlignRightContent: string;
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
  end;


  TAdvStringGridFormatToolBar = class(TAdvStringGridToolBar)
  private
    FOptions: TAdvStringGridToolBarFormatButtons;
    FHints: TAdvStringGridFormatHints;
    procedure SetHints(const Value: TAdvStringGridFormatHints);
  protected
    procedure SetOptions(Value: TAdvStringGridToolBarFormatButtons);
    procedure UpdateButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
  published
    property Hints: TAdvStringGridFormatHints read FHints write SetHints;
    property Options: TAdvStringGridToolBarFormatButtons read FOptions write SetOptions;
  end;

  TAdvStringGridToolBarEditButton = (btFileOpen, btFileSave, btCopy, btPaste, btCut);

  TAdvStringGridToolBarEditButtons = set of TAdvStringGridToolBarEditButton;

  TAdvStringGridEditHints = class(TPersistent)
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


  TAdvStringGridEditToolBar = class(TAdvStringGridToolBar)
  private
    FHints: TAdvStringGridEditHints;
    FOptions: TAdvStringGridToolBarEditButtons;
    procedure SetHints(const Value: TAdvStringGridEditHints);
  protected
    procedure OpenFile(Sender: TObject);
    procedure SaveFile(Sender: TObject);
    procedure SaveToRTF(AFileName: string);

    procedure SetOptions(Value: TAdvStringGridToolBarEditButtons);
    procedure UpdateButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
  published
    property Hints: TAdvStringGridEditHints read FHints write SetHints;
    property Options: TAdvStringGridToolBarEditButtons read FOptions write SetOptions;
  end;

  // Ribbon toolbars

  TAdvStringGridClipboardHints = class(TPersistent)
  private
    FPasteContent: string;
    FCopyTitle: string;
    FCopyContent: string;
    FCutTitle: string;
    FPasteTitle: string;
    FCutContent: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property CutTitle: string read FCutTitle write FCutTitle;
    property CutContent: string read FCutContent write FCutContent;
    property CopyTitle: string read FCopyTitle write FCopyTitle;
    property CopyContent: string read FCopyContent write FCopyContent;
    property PasteTitle: string read FPasteTitle write FPasteTitle;
    property PasteContent: string read FPasteContent write FPasteContent;
  end;

  TAdvStringGridClipboardCaptions = class(TPersistent)
  private
    FCut: string;
    FPaste: string;
    FCopy: string;
    FOnChange: TNotifyEvent;
    procedure SetCopy(const Value: string);
    procedure SetCut(const Value: string);
    procedure SetPaste(const Value: string);
  protected
    procedure DoChanged; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Copy: string read FCopy write SetCopy;
    property Cut: string read FCut write SetCut;
    property Paste: string read FPaste write SetPaste;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvStringGridClipboardRibbonToolBar = class(TAdvStringGridToolBar)
  private
    FCut,FCopy,FPaste: TAdvGlowButton;
    FHints: TAdvStringGridClipboardHints;
    FCaptions: TAdvStringGridClipboardCaptions;
    procedure SetHints(const Value: TAdvStringGridClipboardHints);
    procedure SetCaptions(const Value: TAdvStringGridClipboardCaptions);
  protected
    procedure CreateWnd; override;
    procedure CaptionsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
  published
    property AutoPositionControls default false;
    property Captions: TAdvStringGridClipboardCaptions read FCaptions write SetCaptions;
    property Hints: TAdvStringGridClipboardHints read FHints write SetHints;
  end;

  TAdvStringGridFontHints = class(TPersistent)
  private
    FUnderlineContent: string;
    FItalicTitle: string;
    FBackgroundColorTitle: string;
    FBoldTitle: string;
    FStrikeThroughTitle: string;
    FItalicContent: string;
    FBackgroundColorContent: string;
    FTextColorTitle: string;
    FBoldContent: string;
    FStrikeThroughContent: string;
    FUnderlineTitle: string;
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

    property TextColorTitle: string read FTextColorTitle write FTextColorTitle;
    property TextColorContent: string read FTextColorContent write FTextColorContent;

    property BackgroundColorTitle: string read FBackgroundColorTitle write FBackgroundColorTitle;
    property BackgroundColorContent: string read FBackgroundColorContent write FBackgroundColorContent;
  end;

  TAdvStringGridFontRibbonToolBar = class(TAdvStringGridToolBar)
  private
    FHints: TAdvStringGridFontHints;
    procedure SetHints(const Value: TAdvStringGridFontHints);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
  published
    property Hints: TAdvStringGridFontHints read FHints write SetHints;
  end;

  TAdvStringGridParagraphHints = class(TPersistent)
  private
    FAlignCenterTitle: string;
    FAlignLeftTitle: string;
    FAlignRightTitle: string;
    FAlignCenterContent: string;
    FAlignLeftContent: string;
    FAlignRightContent: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property AlignLeftTitle: string read FAlignLeftTitle write FAlignLeftTitle;
    property AlignLeftContent: string read FAlignLeftContent write FAlignLeftContent;

    property AlignCenterTitle: string read FAlignCenterTitle write FAlignCenterTitle;
    property AlignCenterContent: string read FAlignCenterContent write FAlignCenterContent;

    property AlignRightTitle: string read FAlignRightTitle write FAlignRightTitle;
    property AlignRightContent: string read FAlignRightContent write FAlignRightContent;
  end;

  TAdvStringGridParagraphRibbonToolBar = class(TAdvStringGridToolBar)
  private
    FHints: TAdvStringGridParagraphHints;
    procedure SetHints(const Value: TAdvStringGridParagraphHints);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
  published
    property Hints: TAdvStringGridParagraphHints read FHints write SetHints;
  end;


implementation

uses
  ExtDlgs, Dialogs, PNGImage, JPEG, Math, Windows, AdvGridRTF;

const
  BTNSIZE = 24;

{ TAdvStringGridToolBar }

constructor TAdvStringGridFormatToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
  sep: TAdvToolBarSeparator;
  actn: TAction;
  fs: TAdvOfficeFontSelector;
  fss: TAdvOfficeFontSizeSelector;
  cs: TAdvOfficeColorSelector;

begin
  inherited;

  FHints := TAdvStringGridFormatHints.Create;

  ShowRightHandle := false;

  fs := TAdvOfficeFontSelector.Create(Self);
  fs.OnSelectFontName := SelectFontName;
  fs.OnExit := ExitFontName;
  actn := TAdvStringGridFontName.Create(Self);
  actn.ActionComponent := fs;
  fs.Action := actn;
  fs.Tag := integer(btFontName);
  AddToolBarControl(fs);


  fss := TAdvOfficeFontSizeSelector.Create(Self);
  fss.OnSelectFontSize := SelectFontSize;
  fss.OnExit := ExitFontSize;
  actn := TAdvStringGridFontSize.Create(Self);
  actn.ActionComponent := fss;
  fss.Action := actn;
  fss.Tag := integer(btFontSize);

  AddToolBarControl(fss);

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

  atb := AddButton(HInstance,TAdvStringGridBold, bsCheck, 'TMSRETBBOLD','Bold');
  atb.OfficeHint.Title := Hints.BoldTitle;
  atb.OfficeHint.Notes.Text := Hints.BoldContent;
  atb.Tag := integer(btBold);

  atb := AddButton(HInstance,TAdvStringGridItalic, bsCheck, 'TMSRETBITALIC','Italic');
  atb.OfficeHint.Title := Hints.ItalicTitle;
  atb.OfficeHint.Notes.Text := Hints.ItalicContent;
  atb.Tag := integer(btItalic);

  atb := AddButton(HInstance,TAdvStringGridUnderline, bsCheck, 'TMSRETBUNDERLINE','Underline');
  atb.OfficeHint.Title := Hints.UnderlineTitle;
  atb.OfficeHint.Notes.Text := Hints.UnderlineContent;
  atb.Tag := integer(btUnderline);

  atb := AddButton(HInstance,TAdvStringGridStrikeOut, bsCheck, 'TMSRETBSTRIKE','Strikethrough');
  atb.OfficeHint.Title := Hints.StrikeThroughTitle;
  atb.OfficeHint.Notes.Text := Hints.StrikeThroughContent;
  atb.Tag := integer(btStrikeThrough);

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

  atb := AddButton(HInstance,TAdvStringGridAlignLeft, bsCheck, 'TMSRETBALIGNLEFT','Align left');

  atb.OfficeHint.Title := Hints.AlignLeftTitle;
  atb.OfficeHint.Notes.Text := Hints.AlignLeftContent;
  atb.Tag := integer(btAlignLeft);

  atb := AddButton(HInstance,TAdvStringGridAlignCenter, bscheck, 'TMSRETBALIGNCENTER','Align center');
  atb.OfficeHint.Title := Hints.AlignCenterTitle;
  atb.OfficeHint.Notes.Text := Hints.AlignCenterContent;
  atb.Tag := integer(btAlignCenter);

  atb := AddButton(HInstance,TAdvStringGridAlignRight, bsCheck, 'TMSRETBALIGNRIGHT','Align right');
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

  actn := TAdvStringGridTextColor.Create(Self);
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

  actn := TAdvStringGridColor.Create(Self);
  actn.ActionComponent := cs;
  cs.Action := actn;

  AddToolBarControl(cs);

  Options := [btFontName, btFontSize, btBold, btItalic, btUnderline, btStrikeThrough, btTextColor,
    btBackgroundColor, btAlignLeft, btAlignCenter,  btAlignRight];
end;


destructor TAdvStringGridFormatToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;

{ TAdvStringGridToolBar }

procedure TAdvStringGridToolBar.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FGrid) then
    FGrid := nil;
end;

procedure TAdvStringGridToolBar.SelectColor(Sender: TObject;
  AColor: TColor);
begin
  if Assigned(Grid) then
  begin
//    Grid.SetSelectionBkColor(AColor);
  end;
end;

procedure TAdvStringGridToolBar.SelectFontName(Sender: TObject;
  AName: string);
begin
  if Assigned(Grid) then
  begin
//    Grid.SetSelectionFontName(AName);
    if Grid.Visible then
      Grid.SetFocus;
  end;
end;

procedure TAdvStringGridToolBar.SelectFontSize(Sender: TObject;
  ASize: integer);
begin
  if Assigned(Grid) then
  begin
//    Grid.SetSelectionFontSize(ASize);
    if Grid.Visible then
      Grid.SetFocus;
  end;
end;

procedure TAdvStringGridToolBar.SelectTextColor(Sender: TObject;
  AColor: TColor);
begin
  if Assigned(Grid) then
  begin
//    Grid.SetSelectionColor(AColor);
  end;
end;


procedure TAdvStringGridToolBar.ExitFontName(Sender: TObject);
var
  fn: string;
  i: integer;
begin
  if not Assigned(Grid) then
    Exit;

  if (Sender as TAdvOfficeFontSelector).DroppedDown then
    Exit;

  fn := (Sender as TAdvOfficeFontSelector).Text;

  i := (Sender as TAdvOfficeFontSelector).Items.IndexOf(fn);

  if (i >= 0) then
  begin
    (Sender as TAdvOfficeFontSelector).SelectedFontName := fn;
//    Grid.SetSelectionFontName(fn);
    Grid.SetFocus;
  end;
end;

procedure TAdvStringGridToolBar.ExitFontSize(Sender: TObject);
var
  fs,e: integer;

begin
  if not Assigned(Grid) then
    Exit;

  if (Sender as TAdvOfficeFontSizeSelector).DroppedDown then
    Exit;

  val((Sender as TAdvOfficeFontSizeSelector).Text,fs,e);

  if (e = 0) and (fs > 0) then
  begin
    (Sender as TAdvOfficeFontSizeSelector).SelectedFontSize := fs;
    Grid.SetFocus;
  end;
end;


function TAdvStringGridToolBar.GetButton(Id: integer): TAdvGlowButton;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to ControlCount - 1 do
  begin
    if (Controls[i].Tag = ID) and (Controls[i] is TAdvGlowButton) then
    begin
      Result := Controls[i] as TAdvGlowButton;
    end;
  end;
end;

{ TAdvStringGridEditToolBar }

constructor TAdvStringGridEditToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
  sep: TAdvToolBarSeparator;
begin
  inherited;

  FHints := TAdvStringGridEditHints.Create;

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

  atb := AddButton(HInstance,TAdvStringGridCut, bsButton, 'TMSRETBCUT', 'Cut to clipboard');
  atb.OfficeHint.Title := FHints.CutTitle;
  atb.OfficeHint.Notes.Text := FHints.CutContent;
  atb.Tag := integer(btCut);

  atb := AddButton(HInstance,TAdvStringGridCopy, bsButton, 'TMSRETBCOPY', 'Copy to clipboard');
  atb.OfficeHint.Title := FHints.CopyTitle;
  atb.OfficeHint.Notes.Text := FHints.CopyContent;
  atb.Tag := integer(btCopy);

  atb := AddButton(HInstance,TAdvStringGridPaste, bsButton, 'TMSRETBPASTE', 'Paste from clipboard');
  atb.OfficeHint.Title := FHints.PasteTitle;
  atb.OfficeHint.Notes.Text := FHints.PasteContent;
  atb.Tag := integer(btPaste);

  sep := TAdvToolBarSeparator.Create(Self);
  AddToolBarControl(sep);

//  atb := AddButton(HInstance,TAdvStringGridUndo, bsButton, 'TMSRETBUNDO', 'Undo');
//  atb.OfficeHint.Title := FHints.UndoTitle;
//  atb.OfficeHint.Notes.Text := FHints.UndoContent;
//  atb.Tag := integer(btUndo);
//
//  atb := AddButton(HInstance,TAdvStringGridRedo, bsButton, 'TMSRETBREDO', 'Redo');
//  atb.OfficeHint.Title := FHints.RedoTitle;
//  atb.OfficeHint.Notes.Text := FHints.RedoContent;
//  atb.Tag := integer(btRedo);

  Options := [btFileOpen, btFileSave, btCopy, btPaste, btCut];
end;

destructor TAdvStringGridEditToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;

procedure TAdvStringGridEditToolBar.OpenFile(Sender: TObject);
var
  od: TOpenDialog;
  fe: string;
begin
  if not Assigned(FGrid) then
    raise Exception.Create('No grid assigned');

  od := TOpenDialog.Create(Self);
  od.Filter := 'Text files|*.txt|CSV files|*.csv|All files|*.*';
  try
    if od.Execute then
    begin
      fe := Uppercase(ExtractFileExt(od.FileName));

      if fe = '.TXT' then
        FGrid.LoadFromFile(od.FileName)
      else
        FGrid.LoadFromCSV(od.FileName);
    end;
  finally
    od.Free;
  end;
end;

procedure TAdvStringGridEditToolBar.SaveFile(Sender: TObject);
var
  sd: TSaveDialog;
  fe,fn: string;
begin
  if not Assigned(FGrid) then
    raise Exception.Create('No grid assigned');

  sd := TSaveDialog.Create(Self);
  sd.Filter := 'Text files|*.txt|CSV files|*.csv|HTML files|*.htm|Rich text|*.rtf';
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
          fn := fn + '.csv';
        if sd.FilterIndex = 3 then
          fn := fn + '.htm';
        if sd.FilterIndex = 4 then
          fn := fn + '.rtf';
      end;

      case sd.FilterIndex of
      1: FGrid.SaveToFile(fn);
      2: FGrid.SaveToCSV(fn);
      3: FGrid.SaveToHTML(fn);
      4: SaveToRTF(fn);
      end;
    end;
  finally
    sd.Free;
  end;
end;


procedure TAdvStringGridEditToolBar.SaveToRTF(AFileName: string);
var
  rtf: TAdvGridRTFIO;
begin
  rtf := TAdvGridRTFIO.Create(Self);
  try
    rtf.AdvStringGrid := FGrid;
    rtf.ExportRTF(AFileName);
  finally
    rtf.Free;
  end;
end;

procedure TAdvStringGridFormatToolBar.SetHints(
  const Value: TAdvStringGridFormatHints);
begin
  FHints.Assign(Value);
end;

procedure TAdvStringGridFormatToolBar.SetOptions(
  Value: TAdvStringGridToolBarFormatButtons);
begin
  FOptions := Value;
  UpdateButtons;
end;

procedure TAdvStringGridFormatToolBar.UpdateButtons;
var
  i: integer;
  j: TAdvStringGridToolBarFormatButton;
begin
  for i := 0 to ControlCount - 1 do
  begin
    for j := Low(TAdvStringGridToolBarFormatButton) to High(TAdvStringGridToolBarFormatButton) do
    begin
      if Controls[i].Tag = integer(j) then
        Controls[i].Visible := j in Options;
    end;
  end;
end;

procedure TAdvStringGridFormatToolBar.UpdateHints;
var
  agb: TAdvGlowButton;
begin
  agb := GetButton(integer(btBold));
  agb.OfficeHint.Title := Hints.BoldTitle;
  agb.OfficeHint.Notes.Text := Hints.BoldContent;

  agb := GetButton(integer(btItalic));
  agb.OfficeHint.Title := Hints.ItalicTitle;
  agb.OfficeHint.Notes.Text := Hints.ItalicContent;

  agb := GetButton(integer(btUnderline));
  agb.OfficeHint.Title := Hints.UnderlineTitle;
  agb.OfficeHint.Notes.Text := Hints.UnderlineContent;

  agb := GetButton(integer(btStrikeThrough));
  agb.OfficeHint.Title := Hints.StrikeThroughTitle;
  agb.OfficeHint.Notes.Text := Hints.StrikeThroughContent;

  agb := GetButton(integer(btTextColor));
  agb.OfficeHint.Title := Hints.TextColorTitle;
  agb.OfficeHint.Notes.Text := Hints.TextColorContent;

  agb := GetButton(integer(btBackgroundColor));
  agb.OfficeHint.Title := Hints.BackgroundColorTitle;
  agb.OfficeHint.Notes.Text := Hints.BackgroundColorContent;

  agb := GetButton(integer(btAlignLeft));
  agb.OfficeHint.Title := Hints.AlignLeftTitle;
  agb.OfficeHint.Notes.Text := Hints.AlignLeftContent;

  agb := GetButton(integer(btAlignCenter));
  agb.OfficeHint.Title := Hints.AlignCenterTitle;
  agb.OfficeHint.Notes.Text := Hints.AlignCenterContent;

  agb := GetButton(integer(btAlignRight));
  agb.OfficeHint.Title := Hints.AlignRightTitle;
  agb.OfficeHint.Notes.Text := Hints.AlignRightContent;

end;

procedure TAdvStringGridEditToolBar.SetHints(
  const Value: TAdvStringGridEditHints);
begin
  FHints.Assign(Value);
end;

procedure TAdvStringGridEditToolBar.SetOptions(
  Value: TAdvStringGridToolBarEditButtons);
begin
  FOptions := Value;
  UpdateButtons;
end;

procedure TAdvStringGridEditToolBar.UpdateButtons;
var
  i: integer;
  j: TAdvStringGridToolBarEditButton;
begin
  for i := 0 to ControlCount - 1 do
  begin
    for j := Low(TAdvStringGridToolBarEditButton) to High(TAdvStringGridToolBarEditButton) do
    begin
      if Controls[i].Tag = integer(j) then
        Controls[i].Visible := j in Options;
    end;
  end;
end;

procedure TAdvStringGridEditToolBar.UpdateHints;
var
  agb: TAdvGlowButton;
begin
  agb := GetButton(integer(btCopy));
  agb.OfficeHint.Title := Hints.CopyTitle;
  agb.OfficeHint.Notes.Text := Hints.CopyContent;

  agb := GetButton(integer(btCut));
  agb.OfficeHint.Title := Hints.CutTitle;
  agb.OfficeHint.Notes.Text := Hints.CutContent;

  agb := GetButton(integer(btPaste));
  agb.OfficeHint.Title := Hints.PasteTitle;
  agb.OfficeHint.Notes.Text := Hints.PasteContent;

  agb := GetButton(integer(btFileOpen));
  agb.OfficeHint.Title := Hints.FileOpenTitle;
  agb.OfficeHint.Notes.Text := Hints.FileOpenContent;

  agb := GetButton(integer(btFileSave));
  agb.OfficeHint.Title := Hints.FileSaveTitle;
  agb.OfficeHint.Notes.Text := Hints.FileSaveContent;
end;

{ TAdvStringGridFormatHints }

procedure TAdvStringGridFormatHints.Assign(Source: TPersistent);
begin
  if (Source is TAdvStringGridFormatHints) then
  begin
    FUnderlineContent := (Source as TAdvStringGridFormatHints).UnderlineContent;
    FItalicTitle := (Source as TAdvStringGridFormatHints).ItalicTitle;
    FBackgroundColorTitle := (Source as TAdvStringGridFormatHints).BackgroundColorTitle;
    FAlignCenterTitle := (Source as TAdvStringGridFormatHints).AlignCenterTitle;
    FAlignLeftTitle := (Source as TAdvStringGridFormatHints).AlignLeftTitle;
    FBoldTitle := (Source as TAdvStringGridFormatHints).BoldTitle;
    FStrikeThroughTitle := (Source as TAdvStringGridFormatHints).StrikeThroughTitle;
    FAlignRightTitle := (Source as TAdvStringGridFormatHints).AlignRightTitle;
    FItalicContent := (Source as TAdvStringGridFormatHints).ItalicContent;
    FBackgroundColorContent := (Source as TAdvStringGridFormatHints).BackgroundColorContent;
    FTextColorTitle := (Source as TAdvStringGridFormatHints).TextColorTitle;
    FAlignCenterContent := (Source as TAdvStringGridFormatHints).AlignCenterContent;
    FAlignLeftContent := (Source as TAdvStringGridFormatHints).AlignLeftContent;
    FBoldContent := (Source as TAdvStringGridFormatHints).BoldContent;
    FStrikeThroughContent := (Source as TAdvStringGridFormatHints).StrikeThroughContent;
    FUnderlineTitle := (Source as TAdvStringGridFormatHints).UnderlineTitle;
    FAlignRightContent := (Source as TAdvStringGridFormatHints).AlignRightContent;
    FTextColorContent := (Source as TAdvStringGridFormatHints).TextColorContent;
  end;
end;

constructor TAdvStringGridFormatHints.Create;
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

  FAlignLeftTitle := 'Align text left (Ctrl+L)';
  FAlignLeftContent := 'Align the text to left';

  FAlignCenterTitle := 'Align center (Ctrl+E)';
  FAlignCenterContent := 'Center text';

  FAlignRightTitle := 'Align text right (Ctrl+R)';
  FAlignRightContent := 'Align the text to right';

  FBackgroundColorTitle := 'Background color';
  FBackgroundColorContent := 'Set selection background color';

  FTextColorTitle := 'Text color';
  FTextColorContent := 'Set selection text color';
end;

{ TAdvStringGridEditHints }

procedure TAdvStringGridEditHints.Assign(Source: TPersistent);
begin
  if (Source is TAdvStringGridEditHints) then
  begin
    FPasteContent := (Source as TAdvStringGridEditHints).PasteContent;
    FUndoTitle := (Source as TAdvStringGridEditHints).UndoTitle;
    FRedoTitle := (Source as TAdvStringGridEditHints).RedoTitle;
    FFileSaveContent := (Source as TAdvStringGridEditHints).FileSaveContent;
    FCopyTitle := (Source as TAdvStringGridEditHints).CopyTitle;
    FFileOpenContent := (Source as TAdvStringGridEditHints).FileOpenContent;
    FUndoContent := (Source as TAdvStringGridEditHints).UndoContent;
    FRedoContent := (Source as TAdvStringGridEditHints).RedoContent;
    FCopyContent := (Source as TAdvStringGridEditHints).CopyContent;
    FCutTitle := (Source as TAdvStringGridEditHints).CutTitle;
    FPasteTitle := (Source as TAdvStringGridEditHints).PasteTitle;
    FFileSaveTitle := (Source as TAdvStringGridEditHints).FileSaveTitle;
    FCutContent := (Source as TAdvStringGridEditHints).CutContent;
    FFileOpenTitle := (Source as TAdvStringGridEditHints).FileOpenTitle;
  end;

end;

constructor TAdvStringGridEditHints.Create;
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


{ TAdvStringGridClipboardHints }

procedure TAdvStringGridClipboardHints.Assign(Source: TPersistent);
begin
  if (Source is TAdvStringGridClipboardHints) then
  begin
    FCutTitle := (Source as TAdvStringGridClipboardHints).CutTitle;
    FCutContent := (Source as TAdvStringGridClipboardHints).CutContent;
    FCopyTitle := (Source as TAdvStringGridClipboardHints).CopyTitle;
    FCopyContent := (Source as TAdvStringGridClipboardHints).CopyContent;
    FPasteTitle := (Source as TAdvStringGridClipboardHints).PasteTitle;
    FPasteContent := (Source as TAdvStringGridClipboardHints).PasteContent;
  end;
end;

constructor TAdvStringGridClipboardHints.Create;
begin
  inherited;

  FPasteTitle := 'Paste (Ctrl+V)';
  FPasteContent := 'Add content on the clipboard to grid';

  FCopyTitle := 'Copy (Ctrl+C)';
  FCopyContent := 'Put a copy of the selection on the clipboard';

  FCutTitle := 'Cut (Ctrl+X)';
  FCutContent := 'Remove the selection to the clipboard';
end;



{ TAdvRichEditorClipboardCaptions }

procedure TAdvStringGridClipboardCaptions.Assign(Source: TPersistent);
begin
  if (Source is TAdvStringGridClipboardCaptions) then
  begin
    FCut := (Source as TAdvStringGridClipboardCaptions).Cut;
    FCopy := (Source as TAdvStringGridClipboardCaptions).Copy;
    FPaste := (Source as TAdvStringGridClipboardCaptions).Paste;
  end;
end;

constructor TAdvStringGridClipboardCaptions.Create;
begin
  inherited;
  FCut := 'Cut';
  FCopy := 'Copy';
  FPaste := 'Paste';
end;

procedure TAdvStringGridClipboardCaptions.DoChanged;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvStringGridClipboardCaptions.SetCopy(const Value: string);
begin
  if (FCopy <> Value) then
  begin
    FCopy := Value;
    DoChanged;
  end;
end;

procedure TAdvStringGridClipboardCaptions.SetCut(const Value: string);
begin
  if (FCut <> Value) then
  begin
    FCut := Value;
    DoChanged;
  end;
end;

procedure TAdvStringGridClipboardCaptions.SetPaste(const Value: string);
begin
  if (FPaste <> Value) then
  begin
    FPaste := Value;
    DoChanged;
  end;
end;


{ TAdvStringGridClipboardRibbonToolBar }

procedure TAdvStringGridClipboardRibbonToolBar.CaptionsChanged(Sender: TObject);
begin
  if Assigned(FCut) then
    FCut.Caption := Captions.Cut;
  if Assigned(FCopy) then
    FCopy.Caption := Captions.Copy;
  if Assigned(FPaste) then
    FPaste.Caption := Captions.Paste;
end;

constructor TAdvStringGridClipboardRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
begin
  inherited;

  FHints := TAdvStringGridClipboardHints.Create;
  FCaptions := TAdvStringGridClipboardCaptions.Create;
  FCaptions.OnChange := CaptionsChanged;

  Caption := 'Clipboard';
  CaptionAlignment := taCenter;
  CaptionPosition := cpBottom;
  ShowRightHandle := false;
  ShowCaption := true;
  ShowOptionIndicator := false;

  ToolBarState := tsFixed;
  AutoPositionControls := false;
  AutoSize := false;

  atb := AddButton(HInstance, TAdvStringGridPaste, bsButton, 'TMSRETBPASTELARGE', 'Paste from clipboard');
  atb.Tag := integer(btPaste);
  atb.OfficeHint.Title := FHints.PasteTitle;
  atb.OfficeHint.Notes.Text := FHints.PasteContent;
  atb.Caption := FCaptions.Paste;
  atb.ShowCaption := true;
  atb.DropDownButton := true;
  atb.DropDownPosition := dpBottom;
  atb.MinButtonSizeState := bsLarge;
  atb.MaxButtonSizeState := bsLarge;
  atb.Layout := blGlyphTop;
  atb.Width := 40;
  atb.Height := 64;

  FPaste := atb;

  atb := AddButton(HInstance, TAdvStringGridCut, bsButton, 'TMSRETBCUT', 'Cut to clipboard');
  atb.Caption := 'Cut';
  atb.OfficeHint.Title := FHints.CutTitle;
  atb.OfficeHint.Notes.Text := FHints.CutContent;
  atb.ShowCaption := true;
  atb.Tag := integer(btCut);
  atb.Width := 60;
  atb.Height := 24;
  atb.Left := 41;
  atb.Top := 2;

  FCut := atb;

  atb := AddButton(HInstance, TAdvStringGridCopy, bsButton, 'TMSRETBCOPY', 'Copy to clipboard');
  atb.OfficeHint.Title := FHints.CopyTitle;
  atb.OfficeHint.Notes.Text := FHints.CopyContent;
  atb.Caption := 'Copy';
  atb.ShowCaption := true;
  atb.Tag := integer(btCopy);
  atb.Width := 60;
  atb.Height := 24;
  atb.Left := 41;
  atb.Top := 26;

  FCopy := atb;

  Width := 102;
  Height := 85;
end;

procedure TAdvStringGridClipboardRibbonToolBar.CreateWnd;
begin
  inherited;
  AutoPositionControls := false;
  ToolBarState := tsFixed;
  FCopy.Top := 24;
  FCopy.Left := 41;
end;

destructor TAdvStringGridClipboardRibbonToolBar.Destroy;
begin
  FHints.Free;
  FCaptions.Free;
  inherited;
end;

procedure TAdvStringGridClipboardRibbonToolBar.SetCaptions(
  const Value: TAdvStringGridClipboardCaptions);
begin
  FCaptions := Value;
end;

procedure TAdvStringGridClipboardRibbonToolBar.SetHints(
  const Value: TAdvStringGridClipboardHints);
begin
  FHints := Value;
end;


procedure TAdvStringGridClipboardRibbonToolBar.UpdateHints;
var
  agb: TAdvGlowButton;
begin
  agb := GetButton(integer(btCopy));
  agb.OfficeHint.Title := Hints.CopyTitle;
  agb.OfficeHint.Notes.Text := Hints.CopyContent;

  agb := GetButton(integer(btCut));
  agb.OfficeHint.Title := Hints.CutTitle;
  agb.OfficeHint.Notes.Text := Hints.CutContent;

  agb := GetButton(integer(btPaste));
  agb.OfficeHint.Title := Hints.PasteTitle;
  agb.OfficeHint.Notes.Text := Hints.PasteContent;
end;

{ TAdvStringGridFontHints }

procedure TAdvStringGridFontHints.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TAdvStringGridFontHints.Create;
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

  FBackgroundColorTitle := 'Background color';
  FBackgroundColorContent := 'Set selection background color';

  FTextColorTitle := 'Text color';
  FTextColorContent := 'Set selection text color';
end;


{ TAdvStringGridFontRibbonToolBar }

constructor TAdvStringGridFontRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
  fs: TAdvOfficeFontSelector;
  fss: TAdvOfficeFontSizeSelector;
  cs: TAdvOfficeColorSelector;
  actn: TAction;
begin
  inherited;

  FHints := TAdvStringGridFontHints.Create;

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
  fs.OnExit := ExitFontName;
  actn := TAdvStringGridFontName.Create(Self);
  actn.ActionComponent := fs;
  fs.Action := actn;
  AddToolBarControl(fs);
  fs.Left := 2;
  fs.Top := 2;


  fss := TAdvOfficeFontSizeSelector.Create(Self);
  fss.OnSelectFontSize := SelectFontSize;
  fss.OnExit := ExitFontSize;
  actn := TAdvStringGridFontSize.Create(Self);
  actn.ActionComponent := fss;
  fss.Action := actn;
  AddToolBarControl(fss);
  fss.Left := fs.Left + fs.Width;
  fss.Top := 2;

  atb := AddButton(HInstance, TAdvStringGridBold, bsButton, 'TMSRETBBOLD', 'Bold');
  atb.OfficeHint.Title := FHints.BoldTitle;
  atb.OfficeHint.Notes.Text := FHints.BoldContent;
  atb.Left := 2;
  atb.Top := 26;
  atb.Position := bpLeft;

  atb := AddButton(HInstance, TAdvStringGridItalic, bsButton, 'TMSRETBITALIC', 'Italic');
  atb.OfficeHint.Title := FHints.ItalicTitle;
  atb.OfficeHint.Notes.Text := FHints.ItalicContent;
  atb.Left := 26;
  atb.Top := 26;
  atb.Position := bpMiddle;

  atb := AddButton(HInstance, TAdvStringGridUnderline, bsButton, 'TMSRETBUNDERLINE', 'Underline');
  atb.OfficeHint.Title := FHints.UnderlineTitle;
  atb.OfficeHint.Notes.Text := FHints.UnderlineContent;
  atb.Left := 50;
  atb.Top := 26;
  atb.Position := bpMiddle;

  atb := AddButton(HInstance, TAdvStringGridStrikeout, bsButton, 'TMSRETBSTRIKE', 'Strikeout');
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
  cs.Left := 120;
  cs.Width := 24;
  cs.Height := 24;
  cs.Position := bpLeft;

  actn := TAdvStringGridTextColor.Create(Self);
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
  cs.Left := 144;
  cs.Width := 24;
  cs.Height := 24;
  cs.Position := bpRight;
  AddToolBarControl(cs);

  actn := TAdvStringGridColor.Create(Self);
  actn.ActionComponent := cs;
  cs.Action := actn;

  Width := 175;
  Height := 85;
end;

destructor TAdvStringGridFontRibbonToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;

procedure TAdvStringGridFontRibbonToolBar.SetHints(
  const Value: TAdvStringGridFontHints);
begin
  FHints.Assign(Value);
end;


procedure TAdvStringGridFontRibbonToolBar.UpdateHints;
var
  agb: TAdvGlowButton;

begin
  agb := GetButton(integer(btBold));
  agb.OfficeHint.Title := Hints.BoldTitle;
  agb.OfficeHint.Notes.Text := Hints.BoldContent;

  agb := GetButton(integer(btItalic));
  agb.OfficeHint.Title := Hints.ItalicTitle;
  agb.OfficeHint.Notes.Text := Hints.ItalicContent;

  agb := GetButton(integer(btUnderline));
  agb.OfficeHint.Title := Hints.UnderlineTitle;
  agb.OfficeHint.Notes.Text := Hints.UnderlineContent;

  agb := GetButton(integer(btStrikeThrough));
  agb.OfficeHint.Title := Hints.StrikeThroughTitle;
  agb.OfficeHint.Notes.Text := Hints.StrikeThroughContent;

  agb := GetButton(integer(btTextColor));
  agb.OfficeHint.Title := Hints.TextColorTitle;
  agb.OfficeHint.Notes.Text := Hints.TextColorContent;

  agb := GetButton(integer(btBackgroundColor));
  agb.OfficeHint.Title := Hints.BackgroundColorTitle;
  agb.OfficeHint.Notes.Text := Hints.BackgroundColorContent;

end;

{ TAdvStringGridParagraphHints }

procedure TAdvStringGridParagraphHints.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TAdvStringGridParagraphHints.Create;
begin
  inherited;

  FAlignLeftTitle := 'Align text left (Ctrl+L)';
  FAlignLeftContent := 'Align the text to left';

  FAlignCenterTitle := 'Align center (Ctrl+E)';
  FAlignCenterContent := 'Center text';

  FAlignRightTitle := 'Align text right (Ctrl+R)';
  FAlignRightContent := 'Align the text to right';
end;

{ TAdvStringGridParagraphRibbonToolBar }

constructor TAdvStringGridParagraphRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
begin
  inherited;

  FHints := TAdvStringGridParagraphHints.Create;

  Caption := 'Paragraph';
  CaptionAlignment := taCenter;
  CaptionPosition := cpBottom;
  ShowRightHandle := false;
  ShowCaption := true;
  ShowOptionIndicator := false;

  ToolBarState := tsFixed;
  AutoPositionControls := false;
  AutoSize := false;

  atb := AddButton(HInstance,TAdvStringGridAlignLeft, bsCheck, 'TMSRETBALIGNLEFT','Align left');
  atb.OfficeHint.Title := FHints.AlignLeftTitle;
  atb.OfficeHint.Notes.Text := FHints.AlignLeftContent;
  atb.Tag := integer(btAlignLeft);
  atb.Left := 2;
  atb.Top := 2;
  atb.Position := bpLeft;

  atb := AddButton(HInstance,TAdvStringGridAlignCenter, bscheck, 'TMSRETBALIGNCENTER','Align center');
  atb.OfficeHint.Title := FHints.AlignCenterTitle;
  atb.OfficeHint.Notes.Text := FHints.AlignCenterContent;
  atb.Tag := integer(btAlignCenter);
  atb.Left := 26;
  atb.Top := 2;
  atb.Position := bpMiddle;

  atb := AddButton(HInstance,TAdvStringGridAlignRight, bsCheck, 'TMSRETBALIGNRIGHT','Align right');
  atb.OfficeHint.Title := FHints.AlignRightTitle;
  atb.OfficeHint.Notes.Text := FHints.AlignRightContent;
  atb.Left := 50;
  atb.Top := 2;
  atb.Tag := integer(btAlignRight);
  atb.Position := bpRight;

  Width := 78;
  Height := 85;
end;


destructor TAdvStringGridParagraphRibbonToolBar.Destroy;
begin
  FHints.Free;
  inherited;
end;

procedure TAdvStringGridParagraphRibbonToolBar.SetHints(
  const Value: TAdvStringGridParagraphHints);
begin
  FHints.Assign(Value);
end;

procedure TAdvStringGridParagraphRibbonToolBar.UpdateHints;
var
  agb: TAdvGlowbutton;
begin
  agb := GetButton(integer(btAlignLeft));
  agb.OfficeHint.Title := FHints.AlignLeftTitle;
  agb.OfficeHint.Notes.Text := FHints.AlignLeftContent;

  agb := GetButton(integer(btAlignRight));
  agb.OfficeHint.Title := FHints.AlignRightTitle;
  agb.OfficeHint.Notes.Text := FHints.AlignRightContent;

  agb := GetButton(integer(btAlignCenter));
  agb.OfficeHint.Title := FHints.AlignCenterTitle;
  agb.OfficeHint.Notes.Text := FHints.AlignCenterContent;
end;

end.
