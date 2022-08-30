{***************************************************************************}
{ TMS Spell Check config dialog                                             }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2015                                               }
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
unit TMSSpellCheckConfForm;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  TMSSpellCheck, TMSSpellCheckUtil, ExtDlgs, ExtCtrls;

type
  TSPLCheckConfFrm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    Button3: TButton;
    Button4: TButton;
    Label2: TLabel;
    lsLanguages: TListView;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    GroupBox1: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    GroupBox4: TGroupBox;
    Label7: TLabel;
    Label6: TLabel;
    edLangauge: TComboBox;
    edDescription: TEdit;
    GroupBox3: TGroupBox;
    edWords: TMemo;
    btSaveWords: TButton;
    btLoadWords: TButton;
    edSourceFile: TEdit;
    btSourceFile: TButton;
    edIgnoreList: TMemo;
    OpenDialog: TOpenDialog;
    SpellCheckerOpen: TOpenDialog;
    OpenTextFileDialog1: TOpenTextFileDialog;
    OPENSPlx: TOpenDialog;
    DictOpen: TOpenDialog;
    SAVESPlx: TSaveDialog;
    SaveTextFileDialog1: TSaveTextFileDialog;
    SaveDict: TSaveDialog;
    SaveDialog1: TSaveDialog;
    SaveConfig: TSaveDialog;
    OpenDialog1: TOpenDialog;
    edSoundex: TComboBox;
    Label1: TLabel;
    edProcess: TComboBox;
    Label5: TLabel;
    Panel1: TPanel;
    edRefresh: TButton;
    edSave: TButton;
    Button14: TButton;
    edRetrieve: TButton;
    edAffix: TEdit;
    LbAffix: TButton;
    Label9: TLabel;
    OpenAffix: TOpenDialog;
    procedure Button8Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure edRefreshClick(Sender: TObject);
    procedure edSaveClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure lsLanguagesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btLoadWordsClick(Sender: TObject);
    procedure btSaveWordsClick(Sender: TObject);
    procedure btSourceFileClick(Sender: TObject);
    procedure edRetrieveClick(Sender: TObject);
    procedure lsLanguagesClick(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lsLanguagesItemChecked(Sender: TObject; Item: TListItem);
    procedure Panel1Click(Sender: TObject);
    procedure LbAffixClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure edLangaugeExit(Sender: TObject);
    procedure edDescriptionExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declaratiofns }
    FDontSave: Boolean;
    FSpellCheck: TCustomAdvSpellCheck;
    OldConfig:string;
    procedure ExchangeItems(lv: TListView; const i, j: Integer);
    procedure LoadData(Value: TCustomAdvSpellCheck);
    procedure Save;
    procedure SaveLanguages(Item: TListItem);
  end;

  TAdvSpellCheckConfigDialog = class(TComponent)
  private
    FSpellCheck: TAdvSpellCheck;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    procedure Execute;
  published
    property Spellcheck: TAdvSpellCheck read FSpellCheck write FSpellCheck;
  end;

  TAdvSpellCheckLanguageSelectDialog = class(TComponent)
  private
    FSpellCheck: TAdvSpellCheck;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    procedure Execute;
  published
    property Spellcheck: TAdvSpellCheck read FSpellCheck write FSpellCheck;
  end;


implementation

{$R *.dfm}

uses
  TMSSpellSoundEX;

procedure TSPLCheckConfFrm.btLoadWordsClick(Sender: TObject);
begin
  if OpenTextFileDialog1.Execute then
    edWords.Lines.LoadFromFile(OpenTextFileDialog1.FileName);
end;

procedure TSPLCheckConfFrm.btSaveWordsClick(Sender: TObject);
begin
  if SaveTextFileDialog1.Execute then
    edWords.Lines.SaveToFile(SaveTextFileDialog1.FileName);
end;

procedure TSPLCheckConfFrm.btSourceFileClick(Sender: TObject);
begin
  if OpenTextFileDialog1.Execute then
    edSourceFile.Text := OpenTextFileDialog1.FileName;
end;

procedure TSPLCheckConfFrm.edDescriptionExit(Sender: TObject);
begin
//  if Assigned(lsLanguages.Selected) then
//    lsLanguages.Selected.SubItems[0] := edDescription.Text;
end;

procedure TSPLCheckConfFrm.edLangaugeExit(Sender: TObject);
begin
//  if Assigned(lsLanguages.Selected) then
//    lsLanguages.Selected.Caption := edLangauge.Text;
end;

procedure TSPLCheckConfFrm.edRefreshClick(Sender: TObject);
begin
  PageControl1.Enabled := False;
  Panel1.Enabled := False;
  try
    FSpellCheck.RefreshDatabase;
  finally
    PageControl1.Enabled := True;
    Panel1.Enabled := True;
    Show;
  end;
end;

procedure TSPLCheckConfFrm.edSaveClick(Sender: TObject);
begin
  FSpellCheck.SaveToFile(FSpellCheck.DatabaseFilename + '.SPLX');
end;

procedure TSPLCheckConfFrm.Button14Click(Sender: TObject);
begin
  SaveLanguages(lsLanguages.Selected);
  Save;
  Close;
end;

procedure TSPLCheckConfFrm.Button15Click(Sender: TObject);
var
  index: Integer;
begin
  if Assigned(lsLanguages.Selected) then
  begin
    FDontSave := True;
    SaveLanguages(lsLanguages.Selected);
    if lsLanguages.Selected.index > 0 then
    begin
      index := lsLanguages.Selected.index;
      ExchangeItems(lsLanguages, index - 1,index);
      lsLanguages.Selected := lsLanguages.Items[index-1];
      FSpellCheck.Languages[index].index := index - 1;
    end;
    FDontSave := false;
  end;
end;

procedure TSPLCheckConfFrm.Button16Click(Sender: TObject);
var
  index: Integer;
begin
  if Assigned(lsLanguages.Selected) then
  begin
    FDontSave := true;
    SaveLanguages(lsLanguages.Selected);
    if lsLanguages.Selected.index < lsLanguages.Items.Count then
    begin
      index := lsLanguages.Selected.index;
      FSpellCheck.Languages[index].index := index + 1;

      ExchangeItems(lsLanguages, index, index + 1);
      lsLanguages.Selected := lsLanguages.Items[index + 1];
    end;
    FDontSave := false;
  end;
end;

procedure TSPLCheckConfFrm.Button17Click(Sender: TObject);
var
  lc: TSpellCheckLanguagePack;
  Item: TListItem;
begin
  lc := FSpellCheck.Languages.Add;
  lc.Description := 'New Dictionary ' + IntToStr(lc.index);
  Item := lsLanguages.Items.Add;
  Item.Caption := LanguageEnumToString(lc.LanguageCode);
  Item.SubItems.Add(lc.Description);

  if lc.Enabled then
    Item.SubItems.Add('Enabled')
  else
    Item.SubItems.Add('Disabled');

  Item.Data := lc;
  lsLanguages.Selected := Item;
end;

procedure TSPLCheckConfFrm.Button18Click(Sender: TObject);
begin
  if Assigned(lsLanguages.Selected) then
  begin
    TAdvSpellCheck(lsLanguages.Selected.Data).Free;
    lsLanguages.Selected.Data := nil;
    lsLanguages.Selected.Delete;
  end;
end;

procedure TSPLCheckConfFrm.SaveLanguages(Item: TListItem);
var
  lp: TSpellCheckLanguagePack;
begin
  if Assigned(Item) and Assigned(Item.Data) then
  begin
    lp := TSpellCheckLanguagePack(Item.Data);
    lp.AffixFileName:=edAffix.Text;
    lp.Words.Text := edWords.Lines.Text;
    lp.LanguageCode := StringToLanguageEnum(edLangauge.Text);
    lp.Description := edDescription.Text;
    lp.Enabled := Item.Checked;
    lp.SourceFileName := edSourceFile.Text;
    lp.SoundexName:=edSoundex.Text;
    Item.Caption := LanguageEnumToString(lp.LanguageCode);
    Item.SubItems.Clear;
    Item.SubItems.Add(lp.Description);
    if lp.Enabled then
      Item.SubItems.Add('Enabled')
    else
      Item.SubItems.Add('Disabled');

    case edProcess.ItemIndex of
    0:lp.SoundexProcess := spStandard;
    1:lp.SoundexProcess := spGerman;
    2:lp.SoundexProcess := spFrench;
    3:lp.SoundexProcess := spSpanish;
    4:lp.SoundexProcess := spItalian;
    5:lp.SoundexProcess := spDutch;
    end;
  end;
end;

procedure TSPLCheckConfFrm.ExchangeItems(lv: TListView; const i, j: Integer);
var
  tempLI: TListItem;
begin
  lv.Items.BeginUpdate;
  try
    tempLI := TListItem.Create(lv.Items);
    tempLI.Assign(lv.Items.Item[i]);
    lv.Items.Item[i].Assign(lv.Items.Item[j]);
    lv.Items.Item[j].Assign(tempLI);
    tempLI.Free;
  finally
    lv.Items.EndUpdate
  end;
end;

procedure TSPLCheckConfFrm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if not PageControl1.Enabled then
    CanClose := false;
end;

procedure TSPLCheckConfFrm.Save;
var
  s: boolean;
begin
  s := false;

  if FSpellCheck.IgnoreList.Text<> edIgnoreList.Text then
  begin
    s := true;
    FSpellCheck.IgnoreList.Text := edIgnoreList.Text;
  end;

  if (csDesigning In FSpellCheck.ComponentState) then
    Exit;

  if OldConfig <> FSpellCheck.ReturnXMLConfig then
  begin
    s := true;
    FSpellCheck.RefreshDatabase;
//    FSpellCheck.SaveToFile(FSpellCheck.DatabaseFilename+'.SPLX');
  end;

  if s then
    FSpellCheck.SaveToFile(FSpellCheck.DatabaseFilename+'.SPLX');
end;

procedure TSPLCheckConfFrm.Button1Click(Sender: TObject);
begin
  if SaveTextFileDialog1.Execute then
    edIgnoreList.Lines.SaveToFile
      (FixWithExtension(SaveTextFileDialog1.FileName, '.txt'));
end;

procedure TSPLCheckConfFrm.Button2Click(Sender: TObject);
begin
  if OpenTextFileDialog1.Execute then
    edIgnoreList.Lines.LoadFromFile(OpenTextFileDialog1.FileName);
end;

procedure TSPLCheckConfFrm.Button3Click(Sender: TObject);
begin
  OPENSPlx.FileName := FSpellCheck.DatabaseFilename + '.SPLX';
  if OPENSPlx.Execute then
    FSpellCheck.LoadFromFile(OPENSPlx.FileName);
end;

procedure TSPLCheckConfFrm.Button4Click(Sender: TObject);
begin
  SAVESPlx.FileName := FSpellCheck.DatabaseFilename + '.SPLX';
  if SAVESPlx.Execute then
  begin
    FSpellCheck.SaveToFile(FixWithExtension(SAVESPlx.FileName, '.SPLX'));
  end;
end;

procedure TSPLCheckConfFrm.Button5Click(Sender: TObject);
begin
  if DictOpen.Execute() then
    FSpellCheck.LoadDB(DictOpen.FileName);
end;

procedure TSPLCheckConfFrm.Button6Click(Sender: TObject);
begin
  if SaveDict.Execute then
  begin
    FSpellCheck.SaveDB(FixWithExtension(SaveDict.FileName, '.SPL'));
  end;
end;

procedure TSPLCheckConfFrm.Button7Click(Sender: TObject);
begin
  if SaveConfig.Execute then
    FSpellCheck.SaveConfig(FixWithExtension(SaveConfig.FileName, '.SPLCFG'));
end;

procedure TSPLCheckConfFrm.Button8Click(Sender: TObject);
begin
  if SpellCheckerOpen.Execute then
  begin
    FSpellCheck.LoadConfig(SpellCheckerOpen.FileName);
    LoadData(FSpellCheck);
  end;
end;

procedure TSPLCheckConfFrm.LbAffixClick(Sender: TObject);
begin
  if OpenAffix.Execute() then
  begin
    edAffix.Text := OpenAffix.FileName;
  end;
end;

procedure TSPLCheckConfFrm.edRetrieveClick(Sender: TObject);
begin
{  if FileExists(FSpellCheck.DatabaseFilename + '.SPLX') then
  begin
    FSpellCheck.LoadFromFile(FSpellCheck.DatabaseFilename + '.SPLX');
    LoadData(FSpellCheck);
  end;}
  FSpellCheck.LoadConfigFromString(OldConfig);
  Close;
  //LoadData(FSpellCheck);
end;

procedure TSPLCheckConfFrm.LoadData(Value: TCustomAdvSpellCheck);
var
  i: Integer;
  Item: TListItem;
begin
///  if (csDesigning in Value.ComponentState) then
  lsLanguages.Clear;
  OldConfig:=Value.ReturnXMLConfig;
  begin
    edRefresh.Visible := false;
    edSave.Visible := false;
//    edRetrieve.Visible := false;
  end;

  FSpellCheck := Value;
  edIgnoreList.Text := Value.IgnoreList.Text;

  for I := 0 to Value.SoundexManager.Count-1 do
     edSoundex.Items.Add(Value.SoundexManager[I].Name);

  for i := 0 to Value.Languages.Count - 1 do
  begin
    Item := lsLanguages.Items.Add;
    Item.Caption := LanguageEnumToString(Value.Languages[i].LanguageCode);
    Item.SubItems.Add(Value.Languages[i].Description);
    Item.Checked := Value.Languages[i].Enabled;
    Item.Data := Value.Languages[i];
  end;

  if lsLanguages.Items.Count > 0 then
    lsLanguages.ItemIndex := 0;
end;

procedure TSPLCheckConfFrm.lsLanguagesClick(Sender: TObject);
begin
  if not Assigned(lsLanguages.Selected) then
  begin
    lbAffix.Enabled := false;
    edAffix.Enabled := false;
    edProcess.Enabled := False;
    edWords.Enabled := False;
    edDescription.Enabled := False;
    edLangauge.Enabled := False;
    edSourceFile.Enabled := False;
    btSaveWords.Enabled := False;
    btLoadWords.Enabled := False;
    btSourceFile.Enabled := False;
    edSoundex.Enabled := False;
  end;
end;

procedure TSPLCheckConfFrm.lsLanguagesItemChecked(Sender: TObject;
  Item: TListItem);
begin
  if Assigned(Item) and Assigned(Item.Data) then
    TSpellCheckLanguagePack(Item.Data).Enabled := Item.Checked;
end;

procedure TSPLCheckConfFrm.lsLanguagesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  lp: TSpellCheckLanguagePack;
begin
  if FDontSave then
    Exit;

  if not Selected then
  begin
    if not Item.Deleting then
       SaveLanguages(Item);
    Exit;
  end;

  if Selected then
  begin
    lbAffix.Enabled := true;
    edAffix.Enabled := true;
    edSoundex.Enabled := True;
    edWords.Enabled := True;
    edDescription.Enabled := True;
    edLangauge.Enabled := True;
    edSourceFile.Enabled := True;
    btSaveWords.Enabled := True;
    btLoadWords.Enabled := True;
    btSourceFile.Enabled := True;
    edProcess.Enabled := True;
    lp := TSpellCheckLanguagePack(Item.Data);
    edWords.Text := lp.Words.Text;
    Item.Checked := lp.Enabled;
    edDescription.Text := lp.Description;
    edLangauge.Text := LanguageEnumToString(lp.LanguageCode);
    edSourceFile.Text := lp.SourceFileName;
    edSoundex.Text := lp.SoundexName;
    edAffix.Text := lp.AffixFileName;
    case lp.SoundexProcess of
    spStandard: edProcess.ItemIndex := 0;
    spGerman: edProcess.ItemIndex := 1;
    spFrench: edProcess.ItemIndex := 2;
    spSpanish: edProcess.ItemIndex := 3;
    spItalian: edProcess.ItemIndex := 4;
    spDutch: edProcess.ItemIndex := 5;
    end;
  end;
end;

procedure TSPLCheckConfFrm.PageControl1Change(Sender: TObject);
begin
 if PageControl1.ActivePageIndex=1 then
   LoadData(FSpellCheck);
end;

procedure TSPLCheckConfFrm.Panel1Click(Sender: TObject);
begin

end;

{ TAdvSpellCheckConfigDialog }

procedure TAdvSpellCheckConfigDialog.Execute;
var
  frm: TSPLCheckConfFrm;
begin
  if not Assigned(FSpellCheck) then
    raise Exception.Create(TMSSPELLERRMSG);

  frm := TSPLCheckConfFrm.Create(Application);

  try
    frm.FSpellCheck := FSpellCheck;
    frm.LoadData(FSpellCheck);
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TAdvSpellCheckConfigDialog.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AComponent = FSpellCheck) and (AOperation = opRemove) then
    FSpellCheck := nil;
end;

{ TAdvSpellCheckLanguageSelectDialog }

procedure TAdvSpellCheckLanguageSelectDialog.Execute;
var
  frm: TForm;
  cb: TComboBox;
  lbl: TLabel;
  i: integer;
  btn: TButton;
  bev: TBevel;
begin
  if not Assigned(FSpellCheck) then
    raise Exception.Create('No spell check engine assigned');

  frm := TForm.CreateNew(Application);
  frm.BorderStyle := bsDialog;
  frm.Position := poMainFormCenter;
  frm.Width := 250;
  frm.Height := 135;
  frm.Caption := 'Languages';

  lbl := TLabel.Create(frm);
  lbl.Parent := frm;
  lbl.Left := 16;
  lbl.Top := 8;
  lbl.Caption := 'Select language';

  cb := TComboBox.Create(frm);
  cb.Parent := frm;
  cb.Left := 16;
  cb.Top := 28;
  cb.Width := 160;
  cb.Style := csDropDownList;

  btn := TButton.Create(frm);
  btn.Parent := frm;
  btn.Caption := 'OK';
  btn.Default := true;
  btn.ModalResult := mrOK;
  btn.Top := 76;
  btn.Left := 160;

  btn := TButton.Create(frm);
  btn.Parent := frm;
  btn.Caption := 'Cancel';
  btn.Cancel := true;
  btn.ModalResult := mrCancel;
  btn.Top := 76;
  btn.Left := 80;

  bev := TBevel.Create(frm);
  bev.Parent := frm;
  bev.Left := 0;
  bev.Width := frm.Width;
  bev.Height := 2;
  bev.Top := 70;

  for i := 0 to FSpellCheck.Languages.Count - 1 do
  begin
    cb.Items.Add(FSpellCheck.Languages[i].Description);
  end;

  cb.ItemIndex := FSpellCheck.ActiveLanguageIndex;

  try
    frm.ShowModal;
    FSpellCheck.ActiveLanguageIndex := cb.ItemIndex;
  finally
    frm.Free;

  end;

end;

procedure TAdvSpellCheckLanguageSelectDialog.Notification(
  AComponent: TComponent; AOperation: TOperation);
begin
  inherited;
  if (AComponent = FSpellCheck) and (AOperation = opRemove) then
    FSpellCheck := nil;
end;

end.
