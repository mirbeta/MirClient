unit TokenEditDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, BaseForm, Menus, StdCtrls,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxTokenEdit, cxLabel,
  cxGroupBox, cxTextEdit, cxSpinEdit, cxMaskEdit, cxDropDownEdit, cxCheckBox, dxCheckGroupBox, cxCheckComboBox;

type
  TdxTokenEditDemoForm = class(TfmBaseForm)
    cbCloseGlyphPosition: TcxComboBox;
    cbDisplayMask: TcxComboBox;
    cbGlyphPosition: TcxComboBox;
    cbLookupFilterMode: TcxComboBox;
    chbAllowCustomTokens: TcxCheckBox;
    chbConfirmTokenDeletion: TcxCheckBox;
    chbLookupSorted: TcxCheckBox;
    chbPostOnFocusLeave: TcxCheckBox;
    chbReadOnly: TcxCheckBox;
    chcbLookupFilterSources: TcxCheckComboBox;
    chgbLookup: TdxCheckGroupBox;
    dxTokenEdit: TdxTokenEdit;
    gbMain: TcxGroupBox;
    gbOptions: TcxGroupBox;
    gbSample: TcxGroupBox;
    ilSmall: TcxImageList;
    lbCloseGlyphPosition: TcxLabel;
    lbDisplayMask: TcxLabel;
    lbEditValue: TcxLabel;
    lbEditValueDelimiter: TcxLabel;
    lbGlyphPosition: TcxLabel;
    lbInputDelimiters: TcxLabel;
    lbLookupDropDownRows: TcxLabel;
    lbLookupFilterMode: TcxLabel;
    lbLookupFilterSources: TcxLabel;
    lbMaxLineCount: TcxLabel;
    seLookupDropDownRows: TcxSpinEdit;
    seMaxLineCount: TcxSpinEdit;
    teEditValueDelimiter: TcxTextEdit;
    teInputDelimiters: TcxTextEdit;
    procedure cbCloseGlyphPositionPropertiesEditValueChanged(Sender: TObject);
    procedure cbDisplayMaskPropertiesEditValueChanged(Sender: TObject);
    procedure cbGlyphPositionPropertiesEditValueChanged(Sender: TObject);
    procedure cbLookupFilterModePropertiesEditValueChanged(Sender: TObject);
    procedure chbAllowCustomTokensPropertiesEditValueChanged(Sender: TObject);
    procedure chbLookupSortedPropertiesEditValueChanged(Sender: TObject);
    procedure chbPostOnFocusLeavePropertiesEditValueChanged(Sender: TObject);
    procedure chbReadOnlyPropertiesEditValueChanged(Sender: TObject);
    procedure chcbLookupFilterSourcesPropertiesEditValueChanged(Sender: TObject);
    procedure chgbLookupPropertiesEditValueChanged(Sender: TObject);
    procedure dxTokenEditPropertiesEditValueChanged(Sender: TObject);
    procedure dxTokenEditPropertiesTokenClick(Sender: TObject; const ATokenText: string; AToken: TdxTokenEditToken);
    procedure dxTokenEditPropertiesTokenDelete(Sender: TObject; const ATokenText: string; AToken: TdxTokenEditToken;
      var AAllow: Boolean);
    procedure dxTokenEditPropertiesTokenGlyphClick(Sender: TObject; const ATokenText: string;
      AToken: TdxTokenEditToken);
    procedure dxTokenEditPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure seLookupDropDownRowsPropertiesChange(Sender: TObject);
    procedure seMaxLineCountPropertiesChange(Sender: TObject);
    procedure teEditValueDelimiterPropertiesEditValueChanged(Sender: TObject);
    procedure teInputDelimitersPropertiesEditValueChanged(Sender: TObject);
  end;

var
  dxTokenEditDemoForm: TdxTokenEditDemoForm;

implementation

{$R *.dfm}

procedure TdxTokenEditDemoForm.cbCloseGlyphPositionPropertiesEditValueChanged(Sender: TObject);
begin
  dxTokenEdit.Properties.CloseGlyphPosition := TdxTokenEditElementPosition(cbCloseGlyphPosition.ItemIndex);
end;

procedure TdxTokenEditDemoForm.cbDisplayMaskPropertiesEditValueChanged(Sender: TObject);
begin
  dxTokenEdit.Properties.Lookup.DisplayMask := cbDisplayMask.Text;
  if cbDisplayMask.Properties.Items.IndexOf(cbDisplayMask.Text) = -1 then
    cbDisplayMask.Properties.Items.Add(cbDisplayMask.Text);
end;

procedure TdxTokenEditDemoForm.cbGlyphPositionPropertiesEditValueChanged(Sender: TObject);
begin
  dxTokenEdit.Properties.GlyphPosition := TdxTokenEditElementPosition(cbGlyphPosition.ItemIndex);
end;

procedure TdxTokenEditDemoForm.cbLookupFilterModePropertiesEditValueChanged(Sender: TObject);
begin
  dxTokenEdit.Properties.Lookup.FilterMode := TdxTokenEditLookupFilterMode(cbLookupFilterMode.ItemIndex);
end;

procedure TdxTokenEditDemoForm.chbAllowCustomTokensPropertiesEditValueChanged(Sender: TObject);
begin
  dxTokenEdit.Properties.AllowAddCustomTokens := chbAllowCustomTokens.Checked;
  dxTokenEdit.ValidateEdit;
end;

procedure TdxTokenEditDemoForm.chbLookupSortedPropertiesEditValueChanged(Sender: TObject);
begin
  dxTokenEdit.Properties.Lookup.Sorted := chbLookupSorted.Checked;
end;

procedure TdxTokenEditDemoForm.chbPostOnFocusLeavePropertiesEditValueChanged(Sender: TObject);
begin
  dxTokenEdit.Properties.PostEditValueOnFocusLeave := chbPostOnFocusLeave.Checked;
end;

procedure TdxTokenEditDemoForm.chbReadOnlyPropertiesEditValueChanged(Sender: TObject);
begin
  dxTokenEdit.Properties.ReadOnly := chbReadOnly.Checked;
end;

procedure TdxTokenEditDemoForm.chcbLookupFilterSourcesPropertiesEditValueChanged(Sender: TObject);
var
  AFilterSources: TdxTokenEditLookupFilterSources;
begin
  AFilterSources := [];
  if chcbLookupFilterSources.States[0] = cbsChecked then
    Include(AFilterSources, tefsText);
  if chcbLookupFilterSources.States[1] = cbsChecked then
    Include(AFilterSources, tefsDisplayText);
  dxTokenEdit.Properties.Lookup.FilterSources := AFilterSources;
end;

procedure TdxTokenEditDemoForm.chgbLookupPropertiesEditValueChanged(Sender: TObject);
begin
  dxTokenEdit.Properties.Lookup.Active := chgbLookup.CheckBox.Checked;
end;

procedure TdxTokenEditDemoForm.dxTokenEditPropertiesEditValueChanged(Sender: TObject);
var
  ACaption: string;
begin
  ACaption := 'Edit Value: ';
  if dxTokenEdit.Text <> '' then
    ACaption := ACaption + dxTokenEdit.Text
  else
    ACaption := ACaption + '(empty)';
  lbEditValue.Caption := ACaption;
end;

procedure TdxTokenEditDemoForm.dxTokenEditPropertiesTokenClick(Sender: TObject; const ATokenText: string;
  AToken: TdxTokenEditToken);
begin
  MessageDlg('Clicked token: ' + ATokenText, mtInformation, [mbOK], 0);
end;

procedure TdxTokenEditDemoForm.dxTokenEditPropertiesTokenDelete(Sender: TObject; const ATokenText: string;
  AToken: TdxTokenEditToken; var AAllow: Boolean);
begin
  AAllow := not chbConfirmTokenDeletion.Checked or
    (MessageDlg('Do you want to delete token: ' + ATokenText + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes);
end;

procedure TdxTokenEditDemoForm.dxTokenEditPropertiesTokenGlyphClick(Sender: TObject; const ATokenText: string;
  AToken: TdxTokenEditToken);
begin
  MessageDlg('Clicked glyph of token: ' + ATokenText, mtInformation, [mbOK], 0);
end;

procedure TdxTokenEditDemoForm.dxTokenEditPropertiesValidate(Sender: TObject; var DisplayValue: Variant;
  var ErrorText: TCaption; var Error: Boolean);
var
  AError: Boolean;
begin
  AError := (VarToStr(DisplayValue) <> '') and not chbAllowCustomTokens.Checked and
    (dxTokenEdit.Properties.Tokens.FindByText(DisplayValue) = nil);
  Error := Error or AError;
  if AError then
  begin
    if ErrorText <> '' then
      ErrorText := ErrorText + #13#10;
    ErrorText := ErrorText + 'Custom tokens are not allowed: ' + DisplayValue;
  end;
end;

procedure TdxTokenEditDemoForm.seLookupDropDownRowsPropertiesChange(Sender: TObject);
begin
  if (seLookupDropDownRows.Value <> Null) and (seLookupDropDownRows.Value >= 0) then
    dxTokenEdit.Properties.Lookup.DropDownRows := seLookupDropDownRows.Value
  else
    seLookupDropDownRows.Value := 0;
end;

procedure TdxTokenEditDemoForm.seMaxLineCountPropertiesChange(Sender: TObject);
begin
  if (seMaxLineCount.Value <> Null) and (seMaxLineCount.Value >= 0) then
    dxTokenEdit.Properties.MaxLineCount := seMaxLineCount.Value
  else
    seMaxLineCount.Value := 0;
end;

procedure TdxTokenEditDemoForm.teEditValueDelimiterPropertiesEditValueChanged(Sender: TObject);
begin
  if teEditValueDelimiter.Text <> '' then
    dxTokenEdit.Properties.EditValueDelimiter := teEditValueDelimiter.Text[1]
  else
    teEditValueDelimiter.Text := dxTokenEditDefaultEditValueDelimiter;
  dxTokenEdit.ValidateEdit;
end;

procedure TdxTokenEditDemoForm.teInputDelimitersPropertiesEditValueChanged(Sender: TObject);
begin
  if teInputDelimiters.Text <> '' then
    dxTokenEdit.Properties.InputDelimiters := teInputDelimiters.Text
  else
    teInputDelimiters.Text := dxTokenEditDefaultInputDelimiters;
end;

end.
