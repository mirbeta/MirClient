{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxRichEdit.Dialogs.SearchText;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ActnList, Menus, StdCtrls,
  dxCore, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxLayoutContainer, dxLayoutControl,
  dxLayoutControlAdapters, cxButtons,dxLayoutLookAndFeels, cxClasses, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxMRUEdit, cxCheckBox,

  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Dialogs.FindAndReplaceFormHelpers,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.FindAndReplace,
  dxRichEdit.Dialogs.Core;

type
  { TdxSearchHelper }

  TdxSearchHelper = class(TdxSearchHelperBase)
  strict private
    procedure SubscribeRichEditControlEvents;
    procedure UnsubscribeRichEditControlEvents;
  protected
    function ShouldContinueSearch(const AMessage: string): Boolean; override;
    procedure OnSearchComplete(ASender: TObject; E: TdxSearchCompleteEventArgs); overload;
    procedure OnStopSearching(const AMessage: string); override;
    procedure OnInvalidRegExp(const AMessage: string); override;
    function ShowDialog(const AMessage: string; AFlags: Integer): Integer;
  public
    constructor Create(const AControl: IdxRichEditControl);
    destructor Destroy; override;
  end;

 { TdxFindAndReplaceFormController }

  TdxFindAndReplaceFormController = class(TdxSearchFormControllerBase)
  protected
    function CreateSearchHelper: TdxSearchHelperBase; override;
  public
    procedure ApplyChanges; override;
  end;

  { TdxRichEditSearchTextDialogForm }

  TdxRichEditSearchTextDialogForm = class(TdxRichEditCustomDialogForm)
    alActions: TActionList;
    aFindNext: TAction;
    aFindWholeWord: TAction;
    aMatchCase: TAction;
    aRegex: TAction;
    aReplaceAll: TAction;
    aReplaceNext: TAction;
    btnCancel: TcxButton;
    btnFindNext: TcxButton;
    btnReplaceAll: TcxButton;
    btnReplaceNext: TcxButton;
    cbFndFindWholeWord: TcxCheckBox;
    cbFndMatchCase: TcxCheckBox;
    cbRplFindWholeWord: TcxCheckBox;
    cbRplMatchCase: TcxCheckBox;
    cbRplRegex: TcxCheckBox;
    chbFndRegex: TcxCheckBox;
    cmbFndFindDirection: TcxComboBox;
    cmbRplFindDirection: TcxComboBox;
    dxLayoutControl1Group1: TdxLayoutGroup;
    dxLayoutControl1Group2: TdxLayoutGroup;
    dxLayoutControl1Group3: TdxLayoutGroup;
    dxLayoutControl1Group4: TdxLayoutGroup;
    lciReplaceNext: TdxLayoutItem;
    dxLayoutControl1Item11: TdxLayoutItem;
    lciReplaceAll: TdxLayoutItem;
    dxLayoutControl1Item3: TdxLayoutItem;
    dxLayoutControl1Item4: TdxLayoutItem;
    dxLayoutControl1Item5: TdxLayoutItem;
    dxLayoutControl1Item6: TdxLayoutItem;
    dxLayoutControl1Item7: TdxLayoutItem;
    dxLayoutControl1Item8: TdxLayoutItem;
    dxLayoutControl1Item9: TdxLayoutItem;
    edFndSearchString: TcxMRUEdit;
    edRplReplaceString: TcxMRUEdit;
    edRplSearchString: TcxMRUEdit;
    lcgFind: TdxLayoutGroup;
    lcgReplace: TdxLayoutGroup;
    lcgTabControl: TdxLayoutGroup;
    lciFndSearchString: TdxLayoutItem;
    lcilFndDirection: TdxLayoutItem;
    lciRplDirection: TdxLayoutItem;
    lciRplReplaceString: TdxLayoutItem;
    lciRplSearchString: TdxLayoutItem;
    pmReplaceRegex: TPopupMenu;
    pmSearchRegex: TPopupMenu;
    procedure aReplaceNextExecute(Sender: TObject);
    procedure aReplaceAllExecute(Sender: TObject);
    procedure aFindNextExecute(Sender: TObject);
    procedure aMatchCaseExecute(Sender: TObject);
    procedure aFindWholeWordExecute(Sender: TObject);
    procedure aRegexExecute(Sender: TObject);
    procedure edSearchStringPropertiesButtonClick(Sender: TObject);
    procedure edSearchStringPropertiesChange(Sender: TObject);
    procedure edRplReplaceStringPropertiesButtonClick(Sender: TObject);
    procedure cmbFndFindDirectionPropertiesChange(Sender: TObject);
    procedure cmbRplFindDirectionPropertiesChange(Sender: TObject);
    procedure edRplReplaceStringPropertiesChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure lcgTabControlTabChanged(Sender: TObject);
    procedure SearchStringAddingMRUItem(Sender: TObject; const AText: TCaption);
    procedure ReplaceStringAddingMRUItem(Sender: TObject; const AText: TCaption);
  strict private
    class var
      FSearchStrings: TStrings;
      FReplaceStrings: TStrings;
      FFormPreviousLocation: TPoint;
    class constructor Initialize;
    class destructor Finalize;
  private
    FKeepLocation: Boolean;
    FIsSearchStringChange: Boolean;
    FIsReplaceStringChange: Boolean;
    procedure CheckStateButtons;
    procedure EnableButtons;
    procedure DisableButtons;
    function GetActivePage: TdxSearchFormActivePage; inline;
    procedure CheckSearchStringMRUList;
    procedure CheckReplaceStringMRUList;
    procedure PopulateSearchStringComboBox(AComboBox: TcxCustomComboBox);
    procedure PopulateReplaceStringComboBox(AComboBox: TcxCustomComboBox);
    procedure PopulateDirection;
    procedure RefreshReplaceButtons;
    procedure ReplaceRegexPopupMenuBuild;
    procedure SearchRegexPopupMenuBuild;
    procedure UpdateSearchRegexPopupMenu;
    function GetLastReplaceString: string;
    function GetLastSearchString: string;
    function GetController: TdxFindAndReplaceFormController; inline;
    function GetDocumentModel: TdxDocumentModel; inline;
    function GetSearchStrings: TStrings;
    function GetReplaceStrings: TStrings;
    procedure SetActivePage(const Value: TdxSearchFormActivePage);
    procedure UpdateMRUList(AMRUEdit: TcxMRUEdit; const AText: string);
    class procedure CopyToMRUList(ASource, ADestination: TStrings); static;
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure DoShow; override;
    function GetSearchDirection(AComboBox: TcxComboBox): TdxTextSearchDirection;
    procedure InitializeForm; override;
    procedure RegExButtonsEnabled(AChecked: Boolean);
    procedure UpdateActivePage;
    procedure UpdateFormCore; override;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property SearchStrings: TStrings read GetSearchStrings;
    property ReplaceStrings: TStrings read GetReplaceStrings;
    property LastSearchString: string read GetLastSearchString;
    property LastReplaceString: string read GetLastReplaceString;
  public
    property ActivePage: TdxSearchFormActivePage read GetActivePage write SetActivePage;
    property Controller: TdxFindAndReplaceFormController read GetController;
    property KeepLocation: Boolean read FKeepLocation write FKeepLocation;
  end;

implementation

uses
  Math,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Dialogs.Utils,
  dxTypeHelpers;

{$R *.dfm}

type
  TSearchTextDialogPopupMenuHelper = class helper for TPopupMenu
  public
    function AddMenuItem(AEditor: TcxCustomEdit; ACaptionId: Pointer; const AInsertStr: string;
      AShowInsertStr: Boolean = True): TMenuItem; overload;
    function AddMenuItem(AEditor: TcxCustomEdit; const ACaption, AInsertStr: string;
      AShowInsertStr: Boolean = True): TMenuItem; overload;
    procedure AddMenuSeparator;
    procedure Popup(AControl: TControl);
  end;

{ TdxSearchHelper }

constructor TdxSearchHelper.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  SubscribeRichEditControlEvents;
end;

procedure TdxSearchHelper.SubscribeRichEditControlEvents;
begin
  if (Control <> nil) and (Control.InnerControl <> nil) then
    Control.InnerControl.OnSearchComplete := OnSearchComplete;
end;

procedure TdxSearchHelper.UnsubscribeRichEditControlEvents;
begin
  if (Control <> nil) and (Control.InnerControl <> nil) then
    Control.InnerControl.OnSearchComplete := nil;
end;

procedure TdxSearchHelper.OnSearchComplete(ASender: TObject; E: TdxSearchCompleteEventArgs);
begin
  inherited OnSearchComplete(E);
end;

destructor TdxSearchHelper.Destroy;
begin
  UnsubscribeRichEditControlEvents;
  inherited Destroy;
end;

function TdxSearchHelper.ShouldContinueSearch(const AMessage: string): Boolean;
begin
  Result := ShowDialog(AMessage, MB_YESNO or MB_ICONQUESTION) = ID_YES;
end;

procedure TdxSearchHelper.OnStopSearching(const AMessage: string);
begin
  ShowDialog(AMessage, MB_OK OR MB_ICONINFORMATION);
end;

procedure TdxSearchHelper.OnInvalidRegExp(const AMessage: string);
begin
  ShowDialog(AMessage, MB_OK + MB_ICONERROR);
end;

function TdxSearchHelper.ShowDialog(const AMessage: string; AFlags: Integer): Integer;
begin
  Result := Application.MessageBox(PChar(AMessage), PChar(Application.Title), AFlags);
end;

{ TdxFindAndReplaceFormController }

procedure TdxFindAndReplaceFormController.ApplyChanges;
begin
end;

function TdxFindAndReplaceFormController.CreateSearchHelper: TdxSearchHelperBase;
begin
  Result := TdxSearchHelper.Create(Control);
end;

{ TdxRichEditSearchTextDialogForm }

procedure TdxRichEditSearchTextDialogForm.aRegexExecute(Sender: TObject);
var
  AChecked: Boolean;
begin
  AChecked := aRegex.Checked;
  Controller.RegularExpression := AChecked;
  RegExButtonsEnabled(AChecked);
end;

procedure TdxRichEditSearchTextDialogForm.aReplaceAllExecute(Sender: TObject);
begin
  CheckReplaceStringMRUList;
  Controller.ReplaceAll;
end;

procedure TdxRichEditSearchTextDialogForm.aReplaceNextExecute(Sender: TObject);
begin
  CheckReplaceStringMRUList;
  Controller.Replace;
  btnFindNext.SetFocus;
end;

procedure TdxRichEditSearchTextDialogForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TdxRichEditSearchTextDialogForm.CheckReplaceStringMRUList;
begin
  if not FIsReplaceStringChange then
    Exit;
  if (Controller.ReplaceString <> '') and (ReplaceStrings.IndexOf(Controller.ReplaceString) = -1) then
    ReplaceStringAddingMRUItem(nil, Controller.ReplaceString);
  FIsReplaceStringChange := False;
end;

procedure TdxRichEditSearchTextDialogForm.CheckSearchStringMRUList;
begin
  if not FIsSearchStringChange then
    Exit;
  if SearchStrings.IndexOf(Controller.SearchString) = -1 then
    SearchStringAddingMRUItem(nil, Controller.SearchString);
  FIsSearchStringChange := False;
end;

procedure TdxRichEditSearchTextDialogForm.CheckStateButtons;
begin
  if Controller.SearchString = '' then
    DisableButtons
  else
    EnableButtons;
end;

procedure TdxRichEditSearchTextDialogForm.cmbFndFindDirectionPropertiesChange(Sender: TObject);
begin
  Controller.Direction := GetSearchDirection(cmbFndFindDirection);
end;

procedure TdxRichEditSearchTextDialogForm.cmbRplFindDirectionPropertiesChange(Sender: TObject);
begin
  Controller.Direction := GetSearchDirection(cmbRplFindDirection);
end;

class procedure TdxRichEditSearchTextDialogForm.CopyToMRUList(ASource, ADestination: TStrings);
var
  ACount, I: Integer;
begin
  ACount := ASource.Count;
  ADestination.BeginUpdate;
  for I := ACount - 1 downto 0 do
    ADestination.Add(ASource[I]);
  ADestination.EndUpdate;
end;

function TdxRichEditSearchTextDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxFindAndReplaceFormController.Create(AControllerParameters as TdxSearchFormControllerParameters);
end;

procedure TdxRichEditSearchTextDialogForm.CreateParams(var Params: TCreateParams);
var
  AParentForm: TCustomForm;
begin
  inherited CreateParams(Params);
  AParentForm := GetParentForm(TControl(Owner));
  if AParentForm <> nil then
    Params.WndParent := AParentForm.Handle;
end;

procedure TdxRichEditSearchTextDialogForm.DisableButtons;
begin
  aFindNext.Enabled := false;
  aReplaceNext.Enabled := false;
  aReplaceAll.Enabled := false;
end;

procedure TdxRichEditSearchTextDialogForm.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
  if KeepLocation then
    FFormPreviousLocation.Init(Left, Top);
end;

procedure TdxRichEditSearchTextDialogForm.DoShow;
begin
  inherited DoShow;
  if Assigned(Controller) and Control.ReadOnly then
    lcgReplace.Visible := False;
  UpdateActivePage;
end;

procedure TdxRichEditSearchTextDialogForm.RegExButtonsEnabled(AChecked: Boolean);
begin
  edFndSearchString.Properties.Buttons[1].Enabled := AChecked;
  edRplSearchString.Properties.Buttons[1].Enabled := AChecked;
  edRplReplaceString.Properties.Buttons[1].Enabled := AChecked;
end;

procedure TdxRichEditSearchTextDialogForm.edSearchStringPropertiesChange(Sender: TObject);
var
  AText: string;
begin
  AText := (Sender as TcxMRUEdit).Text;
  if Controller.SearchString = AText then
    Exit;
  Controller.SearchString := AText;
  CheckStateButtons;
  FIsSearchStringChange := True;
end;

procedure TdxRichEditSearchTextDialogForm.SearchStringAddingMRUItem(Sender: TObject;
  const AText: TCaption);
begin
  SearchStrings.Add(AText);
  if ActivePage = TdxSearchFormActivePage.Find then
    UpdateMRUList(edRplSearchString, AText)
  else
    UpdateMRUList(edFndSearchString, AText)
end;

procedure TdxRichEditSearchTextDialogForm.edRplReplaceStringPropertiesButtonClick(Sender: TObject);
begin
  if aRegex.Checked then
    pmReplaceRegex.Popup(Sender as TControl);
end;

procedure TdxRichEditSearchTextDialogForm.edRplReplaceStringPropertiesChange(Sender: TObject);
begin
  Controller.ReplaceString := VarToStr((Sender as TcxCustomEdit).EditingValue);
  FIsReplaceStringChange := True;
end;

procedure TdxRichEditSearchTextDialogForm.ReplaceStringAddingMRUItem(Sender: TObject;
  const AText: TCaption);
begin
  ReplaceStrings.Add(AText);
  UpdateMRUList(edRplReplaceString, AText);
end;

procedure TdxRichEditSearchTextDialogForm.edSearchStringPropertiesButtonClick(Sender: TObject);
begin
  if aRegex.Checked then
  begin
    UpdateSearchRegexPopupMenu;
    pmSearchRegex.Popup(Sender as TControl);
  end;
end;

procedure TdxRichEditSearchTextDialogForm.EnableButtons;
begin
  aFindNext.Enabled := True;
  aReplaceNext.Enabled := True;
  aReplaceAll.Enabled := True;
end;

class destructor TdxRichEditSearchTextDialogForm.Finalize;
begin
  FSearchStrings.Free;
  FReplaceStrings.Free;
end;

procedure TdxRichEditSearchTextDialogForm.aFindNextExecute(Sender: TObject);
begin
  CheckSearchStringMRUList;
  Controller.Find;
end;

procedure TdxRichEditSearchTextDialogForm.aFindWholeWordExecute(Sender: TObject);
begin
  Controller.FindWholeWord := aFindWholeWord.Checked;
end;

procedure TdxRichEditSearchTextDialogForm.aMatchCaseExecute(Sender: TObject);
begin
  Controller.CaseSensitive := aMatchCase.Checked;
end;

function TdxRichEditSearchTextDialogForm.GetController: TdxFindAndReplaceFormController;
begin
  Result := TdxFindAndReplaceFormController(inherited Controller);
end;

function TdxRichEditSearchTextDialogForm.GetDocumentModel: TdxDocumentModel;
begin
  Result := Control.DocumentModel;
end;

function TdxRichEditSearchTextDialogForm.GetLastReplaceString: string;
begin
  if ReplaceStrings.Count > 0 then
    Result := ReplaceStrings[ReplaceStrings.Count - 1]
  else
    Result := '';
end;

function TdxRichEditSearchTextDialogForm.GetLastSearchString: string;
begin
  if SearchStrings.Count > 0 then
    Result := SearchStrings[SearchStrings.Count - 1]
  else
    Result := '';
end;

function TdxRichEditSearchTextDialogForm.GetReplaceStrings: TStrings;
begin
  Result := FReplaceStrings;
end;

function TdxRichEditSearchTextDialogForm.GetActivePage: TdxSearchFormActivePage;
begin
  Result := TdxSearchFormActivePage(lcgTabControl.ItemIndex);
end;

function TdxRichEditSearchTextDialogForm.GetSearchDirection(AComboBox: TcxComboBox): TdxTextSearchDirection;
begin
  Result := TdxTextSearchDirection(AComboBox.ItemIndex);
end;

function TdxRichEditSearchTextDialogForm.GetSearchStrings: TStrings;
begin
  Result := FSearchStrings;
end;

procedure TdxRichEditSearchTextDialogForm.PopulateDirection;
begin
  Populate(cmbFndFindDirection, procedure(ACombobox: TcxCustomComboBox)
    var
      P: Pointer;
    begin
      for P in dxSearchTextDirectionNames do
        ACombobox.Properties.Items.Add(cxGetResourceString(P));
    end);
  cmbRplFindDirection.Properties.Items.Assign(cmbFndFindDirection.Properties.Items);
  cmbFndFindDirection.ItemIndex := Ord(TdxTextSearchDirection.All);
  cmbRplFindDirection.ItemIndex := Ord(TdxTextSearchDirection.All);
end;

procedure TdxRichEditSearchTextDialogForm.PopulateReplaceStringComboBox(AComboBox: TcxCustomComboBox);
begin
  CopyToMRUList(ReplaceStrings, AComboBox.ActiveProperties.LookupItems);
end;

procedure TdxRichEditSearchTextDialogForm.PopulateSearchStringComboBox(AComboBox: TcxCustomComboBox);
begin
  CopyToMRUList(SearchStrings, AComboBox.ActiveProperties.LookupItems);
end;

procedure TdxRichEditSearchTextDialogForm.RefreshReplaceButtons;
var
  AVisible: Boolean;
begin
  AVisible := ActivePage = TdxSearchFormActivePage.Replace;
  lciReplaceNext.Visible := AVisible;
  lciReplaceAll.Visible := AVisible;
end;

procedure TdxRichEditSearchTextDialogForm.ReplaceRegexPopupMenuBuild;
var
  ATaggedExpression: string;
  I: Integer;
begin
  ATaggedExpression := cxGetResourceString(@sdxRichEditSearchTextDialogTaggedExpression);
  for I := 1 to 9 do
    pmReplaceRegex.AddMenuItem(edRplReplaceString, Format('%s %d', [ATaggedExpression, I]), Format('$%d', [I]), False);
end;

procedure TdxRichEditSearchTextDialogForm.SearchRegexPopupMenuBuild;
begin
  pmSearchRegex.AddMenuItem(nil, cxGetResourceString(@sdxRichEditSearchTextDialogAnySingleCharacter), '.');
  pmSearchRegex.AddMenuItem(nil, cxGetResourceString(@sdxRichEditSearchTextDialogZeroOrMore), '*');
  pmSearchRegex.AddMenuItem(nil, cxGetResourceString(@sdxRichEditSearchTextDialogOneOrMore), '+');
  pmSearchRegex.AddMenuItem(nil, cxGetResourceString(@sdxRichEditSearchTextDialogBeginningOfLine), '^');
  pmSearchRegex.AddMenuSeparator;
  pmSearchRegex.AddMenuItem(nil, cxGetResourceString(@sdxRichEditSearchTextDialogEndOfLine), '$');
  pmSearchRegex.AddMenuItem(nil, cxGetResourceString(@sdxRichEditSearchTextDialogBeginningOfWord), '\b');
  pmSearchRegex.AddMenuItem(nil, cxGetResourceString(@sdxRichEditSearchTextDialogAnyOneCharacterInTheSet), '[ ]');
  pmSearchRegex.AddMenuSeparator;
  pmSearchRegex.AddMenuItem(nil, cxGetResourceString(@sdxRichEditSearchTextDialogAnyOneCharacterNotInTheSet), '[^]');
  pmSearchRegex.AddMenuItem(nil, cxGetResourceString(@sdxRichEditSearchTextDialogOr), '|');
  pmSearchRegex.AddMenuItem(nil, cxGetResourceString(@sdxRichEditSearchTextDialogEscapeSpecialCharacter), '\');
  pmSearchRegex.AddMenuItem(nil, cxGetResourceString(@sdxRichEditSearchTextDialogTagExpression), '( )');
  pmSearchRegex.AddMenuItem(nil, cxGetResourceString(@sdxRichEditSearchTextDialogWordCharacter), '\w');
  pmSearchRegex.AddMenuSeparator;
  pmSearchRegex.AddMenuItem(nil, cxGetResourceString(@sdxRichEditSearchTextDialogSpaceOrTab), '\s');
  pmSearchRegex.AddMenuItem(nil, cxGetResourceString(@sdxRichEditSearchTextDialogInteger), '\d');
end;

procedure TdxRichEditSearchTextDialogForm.SetActivePage(const Value: TdxSearchFormActivePage);
begin
  lcgTabControl.ItemIndex := Integer(Value);
end;

procedure TdxRichEditSearchTextDialogForm.UpdateActivePage;
begin
  case ActivePage of
    TdxSearchFormActivePage.Find:
      begin
        edFndSearchString.SetFocus;
        edFndSearchString.EditValue := Controller.SearchString;
        Controller.Direction := GetSearchDirection(cmbFndFindDirection);
      end;
    TdxSearchFormActivePage.Replace:
      begin
        edRplSearchString.SetFocus;
        edRplSearchString.EditValue := Controller.SearchString;
        edRplReplaceString.EditValue := Controller.ReplaceString;
        Controller.Direction := GetSearchDirection(cmbRplFindDirection);
      end;
  end;
  aMatchCase.Checked := Controller.CaseSensitive;
  aFindWholeWord.Checked := Controller.FindWholeWord;
  aRegex.Checked := Controller.RegularExpression;
  RegExButtonsEnabled(Controller.RegularExpression);
end;

procedure TdxRichEditSearchTextDialogForm.UpdateFormCore;
var
  ASearchString: string;
begin
  if Controller.TryGetSearchStringFromSelection(ASearchString) then
  begin
    edFndSearchString.EditValue := ASearchString;
    edRplSearchString.EditValue := ASearchString;
  end
  else
  begin
    edFndSearchString.EditValue := LastSearchString;
    edRplSearchString.EditValue := LastSearchString;
    if DocumentModel.Selection.Length > 0 then
    begin
      cmbFndFindDirection.ItemIndex := Integer(TdxTextSearchDirection.Down);
      cmbRplFindDirection.ItemIndex := Integer(TdxTextSearchDirection.Down);
    end;
  end;
  edRplReplaceString.EditValue := LastReplaceString;
  CheckStateButtons;
end;

procedure TdxRichEditSearchTextDialogForm.UpdateMRUList(AMRUEdit: TcxMRUEdit; const AText: string);
var
  AList: TStrings;
  I: Integer;
begin
  AList := AMRUEdit.ActiveProperties.LookupItems;
  AList.BeginUpdate;
  try
    I := AList.IndexOf(AText);
    if I > 0 then
      AList.Delete(I);
    if I <> 0 then
      AList.Insert(0, AText);
  finally
    AList.EndUpdate;
  end;
end;

procedure TdxRichEditSearchTextDialogForm.UpdateSearchRegexPopupMenu;
var
  AMenuItem: TMenuItem;
  ActiveEditor: TcxCustomEdit;
begin
  if lcgTabControl.ItemIndex = Ord(TdxSearchFormActivePage.Find) then
    ActiveEditor := edFndSearchString
  else
    ActiveEditor := edRplSearchString;
  for AMenuItem in pmSearchRegex.Items do
    if AMenuItem is TInsertRegexMenuItem then
      TInsertRegexMenuItem(AMenuItem).ItemCommand.Editor := ActiveEditor;
end;

procedure TdxRichEditSearchTextDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditSearchTextDialogForm);
  aReplaceNext.Caption := cxGetResourceString(@sdxRichEditSearchTextDialogButtonReplaceNext);
  aReplaceAll.Caption := cxGetResourceString(@sdxRichEditSearchTextDialogButtonReplaceAll);
  aFindNext.Caption := cxGetResourceString(@sdxRichEditSearchTextDialogButtonFindNext);

  aMatchCase.Caption := cxGetResourceString(@sdxRichEditSearchTextDialogMatchCase);
  aFindWholeWord.Caption := cxGetResourceString(@sdxRichEditSearchTextDialogFindWholeWord);
  aRegex.Caption := cxGetResourceString(@sdxRichEditSearchTextDialogRegex);

  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  lcgFind.CaptionOptions.Text := cxGetResourceString(@sdxRichEditSearchTextDialogFind);
  lcgReplace.CaptionOptions.Text := cxGetResourceString(@sdxRichEditSearchTextDialogReplace);
  lciFndSearchString.CaptionOptions.Text := cxGetResourceString(@sdxRichEditSearchTextDialogSearchString);
  lcilFndDirection.CaptionOptions.Text := cxGetResourceString(@sdxRichEditSearchTextDialogDirection);
  lciRplSearchString.CaptionOptions.Text := cxGetResourceString(@sdxRichEditSearchTextDialogSearchString);
  lciRplReplaceString.CaptionOptions.Text := cxGetResourceString(@sdxRichEditSearchTextDialogRplReplaceString);
  lciRplDirection.CaptionOptions.Text := cxGetResourceString(@sdxRichEditSearchTextDialogDirection);

  PopulateDirection;
end;

class constructor TdxRichEditSearchTextDialogForm.Initialize;
begin
  FSearchStrings := TStringList.Create;
  FReplaceStrings := TStringList.Create;
  FFormPreviousLocation.Init(MinInt, MinInt);
end;

procedure TdxRichEditSearchTextDialogForm.InitializeForm;
begin
  FKeepLocation := True;
  if not FFormPreviousLocation.IsEqual(TPoint.Create(MinInt, MinInt)) then
  begin
    Left := FFormPreviousLocation.X;
    Top := FFormPreviousLocation.Y;
  end
  else
    Position := poMainFormCenter;
  SearchRegexPopupMenuBuild;
  ReplaceRegexPopupMenuBuild;

  Populate(edFndSearchString, PopulateSearchStringComboBox);
  Populate(edRplSearchString, PopulateSearchStringComboBox);
  Populate(edRplReplaceString, PopulateReplaceStringComboBox);
end;

procedure TdxRichEditSearchTextDialogForm.lcgTabControlTabChanged(Sender: TObject);
begin
  RefreshReplaceButtons;
  if Visible then
    UpdateActivePage;
end;

{ TSearchTextDialogPopupMenuHelper }

function TSearchTextDialogPopupMenuHelper.AddMenuItem(AEditor: TcxCustomEdit; ACaptionId: Pointer; const AInsertStr: string;
  AShowInsertStr: Boolean): TMenuItem;
begin
  Result := AddMenuItem(AEditor, cxGetResourceString(ACaptionId), AInsertStr, AShowInsertStr);
end;

function TSearchTextDialogPopupMenuHelper.AddMenuItem(AEditor: TcxCustomEdit; const ACaption, AInsertStr: string;
  AShowInsertStr: Boolean): TMenuItem;
begin
  Result := TInsertRegexMenuItem.Create(Self, nil, AEditor, ACaption, AInsertStr, AShowInsertStr);
  Items.Add(Result);
end;

procedure TSearchTextDialogPopupMenuHelper.AddMenuSeparator;
var
  AMenuItem: TMenuItem;
begin
  AMenuItem := TMenuItem.Create(Self);
  AMenuItem.Caption := '-';
  Self.Items.Add(AMenuItem);
end;

procedure TSearchTextDialogPopupMenuHelper.Popup(AControl: TControl);
var
  R: TRect;
  P: TPoint;
begin
  R := AControl.ClientRect;
  P := AControl.ClientToScreen(Point(R.Right, R.Top));
  inherited Popup(P.X, P.Y);
end;

end.
