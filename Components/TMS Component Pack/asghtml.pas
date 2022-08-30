{********************************************************************}
{ TAdvGridHTMLSettingsDialog component                               }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by    Christopher Sansone, ScholarSoft                     }
{               Web : http://www.meteortech.com/ScholarSoft/         }
{ enhanced by : TMS Software                                         }
{               copyright © 1998-2015                                }
{               Email : info@tmssoftware.com                         }
{               Web : http://www.tmssoftware.com                     }
{********************************************************************}

unit AsgHTML;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, AdvSpin, AdvGrid, Buttons, shellapi, Mask;

type
  TAdvGridHTMLSettingsForm = class(TForm)
    CellsGroupBox: TGroupBox;
    BorderSizeLabel: TLabel;
    CellSpacingLabel: TLabel;
    TagsGroupBox: TGroupBox;
    PrefixLabel: TLabel;
    SuffixLabel: TLabel;
    TableStyleLabel: TLabel;
    FilesGroupBox: TGroupBox;
    HeaderLabel: TLabel;
    FooterLabel: TLabel;
    GeneralGroupBox: TGroupBox;
    TableColorsCheckBox: TCheckBox;
    TableFontsCheckBox: TCheckBox;
    TableWidthLabel: TLabel;
    BorderSizeSpinEdit: TAdvSpinEdit;
    CellSpacingSpinEdit: TAdvSpinEdit;
    PrefixEdit: TEdit;
    SuffixEdit: TEdit;
    TableStyleEdit: TEdit;
    HeaderEdit: TEdit;
    FooterEdit: TEdit;
    HeaderButton: TButton;
    FooterButton: TButton;
    TableWidthSpinEdit: TAdvSpinEdit;
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    OpenDialog: TOpenDialog;
    Preview: TButton;
    Label1: TLabel;
    CellPaddingSpinEdit: TAdvSpinEdit;
    ConvertChar: TCheckBox;
    ExportNonBreaking: TCheckBox;
    AutoShow: TCheckBox;
    GroupBox1: TGroupBox;
    ExportImg: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    ImgFolder: TEdit;
    ImgBaseName: TEdit;
    procedure UpdateControls;
    procedure UpdateSettings;
    procedure HeaderButtonClick(Sender: TObject);
    procedure PreviewClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Grid: TAdvStringGrid;
    procedure EnableGroupBox(AGroupBox: TGroupBox; Enable: Boolean);
  end;

  TAdvGridHTMLOption = (hoGeneral, hoCells, hoTags, hoFiles);
  TAdvGridHTMLOptions = set of TAdvGridHTMLOption;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGridHTMLSettingsDialog = class(TCommonDialog)
  private
    FGrid: TAdvStringGrid;
    FForm: TAdvGridHTMLSettingsForm;
    FOptions: TAdvGridHTMLOptions;
  protected
    procedure EnableGroupBoxes;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    property Form: TAdvGridHTMLSettingsForm read FForm;
  published
    property Grid: TAdvStringGrid read FGrid write FGrid;
    property Options: TAdvGridHTMLOptions read FOptions write FOptions;
  end;


implementation


{$R *.DFM}

procedure TAdvGridHTMLSettingsForm.UpdateControls;
begin

  with Grid.HTMLSettings do
  begin
    PrefixEdit.Text := PrefixTag;
    SuffixEdit.Text := SuffixTag;
    TableStyleEdit.Text := TableStyle;

    HeaderEdit.Text := HeaderFile;
    FooterEdit.Text := FooterFile;

    BorderSizeSpinEdit.Value := BorderSize;
    CellSpacingSpinEdit.Value := CellSpacing;
    CellPaddingSpinEdit.Value := CellPadding;

    TableWidthSpinEdit.Value := Width;
    TableColorsCheckBox.Checked := SaveColor;
    TableFontsCheckBox.Checked := SaveFonts;

    ConvertChar.Checked := ConvertSpecialChars;
    ExportNonBreaking.Checked := NonBreakingText;
    AutoShow.Checked := AutoPreview;

    ImgFolder.Text := ImageFolder;
    ImgBaseName.Text := ImageBaseName;
    ExportImg.Checked := ExportImages;
  end;
end;

procedure TAdvGridHTMLSettingsForm.UpdateSettings;
begin
  with Grid.HTMLSettings do
  begin
    PrefixTag := PrefixEdit.Text;
    SuffixTag := SuffixEdit.Text;
    TableStyle := TableStyleEdit.Text;

    HeaderFile := HeaderEdit.Text;
    FooterFile := FooterEdit.Text;

    BorderSize := BorderSizeSpinEdit.Value;
    CellSpacing := CellSpacingSpinEdit.Value;
    CellPadding := CellPaddingSpinEdit.Value;

    Width := TableWidthSpinEdit.Value;
    SaveColor := TableColorsCheckBox.Checked;
    SaveFonts := TableFontsCheckBox.Checked;

    AutoPreview := AutoShow.Checked;
    ConvertSpecialChars := ConvertChar.Checked;
    NonBreakingText := ExportNonBreaking.Checked;

    ImageFolder := ImgFolder.Text;
    ImageBaseName := ImgBaseName.Text;
    ExportImages := ExportImg.Checked;
  end;
end;

procedure TAdvGridHTMLSettingsForm.EnableGroupBox(AGroupBox: TGroupBox;
                                                  Enable: Boolean);
var
  i: integer;
begin
  With AGroupBox do begin
    Enabled := Enable;
    For i := 0 to ControlCount - 1 do
      Controls[i].Enabled := Enable;
  end;
end;

procedure TAdvGridHTMLSettingsForm.HeaderButtonClick(Sender: TObject);
var
  FileEdit: TEdit;
begin
  with OpenDialog do
  begin
    if Sender = HeaderButton then
      FileEdit := HeaderEdit
    else
      FileEdit := FooterEdit;

    FileName := FileEdit.Text;
    if Execute then
      FileEdit.Text := FileName;
  end;
end;

constructor TAdvGridHTMLSettingsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOptions := [hoGeneral, hoCells, hoTags, hoFiles];

  If (csDesigning in ComponentState) then
    FForm := TAdvGridHTMLSettingsForm.Create(Application)
  else
    FForm := TAdvGridHTMLSettingsForm.Create(Self);
end;

function TAdvGridHTMLSettingsDialog.Execute: Boolean;
begin
  if not Assigned(Grid) then
  begin
    raise Exception.Create('The dialog does not have a grid component assigned.');
    Exit;
  end;

  with FForm do
  begin
    Grid := Self.Grid;
    UpdateControls;
    EnableGroupBoxes;

    Result := ShowModal = mrOK;
    if Result then
      UpdateSettings;
  end;
end;

destructor TAdvGridHTMLSettingsDialog.Destroy;
begin
  FForm.Free;
  inherited;
end;

procedure TAdvGridHTMLSettingsDialog.EnableGroupBoxes;
begin
  With FForm do begin
    EnableGroupBox(GeneralGroupBox, hoGeneral in FOptions);
    EnableGroupBox(CellsGroupBox, hoCells in FOptions);
    EnableGroupBox(TagsGroupBox, hoTags in FOptions);
    EnableGroupBox(FilesGroupBox, hoFiles in FOptions);
  end;
end;

procedure TAdvGridHTMLSettingsDialog.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
 if (aOperation=opRemove) and (aComponent=fGrid) then fGrid:=nil;
 inherited;
end;

function AddBackslash(const s: string): string;
begin
  if (Length(s) >= 1) and (s[Length(s)]<>'\') then
    Result := s + '\'
  else
    Result := s;
end;

function WinTempDir: string;
var
  buf:string;
  i: integer;
begin
  SetLength(buf, MAX_PATH);
  i := GetTempPath(Length(buf), PChar(buf));
  SetLength(buf, i);
  Result := AddBackslash(buf);
end;

procedure TAdvGridHTMLSettingsForm.PreviewClick(Sender: TObject);
var
  fname:string;
begin
  UpdateSettings;

  fname := WinTempDir + 'temp001.htm';
  grid.SaveToHTML(fname);
  ShellExecute(0,'open',PChar(fname),nil,nil, SW_NORMAL);
end;


end.
