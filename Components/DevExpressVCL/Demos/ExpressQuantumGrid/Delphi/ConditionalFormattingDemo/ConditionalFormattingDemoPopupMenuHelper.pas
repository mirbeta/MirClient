unit ConditionalFormattingDemoPopupMenuHelper;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dxBar, dxRibbon, ImgList, cxImageList, cxGraphics,
  dxBarExtItems, dxGallery, dxRibbonGallery, cxClasses,
  cxDataControllerConditionalFormatting,
  dxSpreadSheetConditionalFormattingRules,
  dxSpreadSheetConditionalFormatting,
  cxDataControllerConditionalFormattingRulesManagerDialog;

type
  TConditionalFormattingPopupMenuHelper = class(TForm)
    dxBarManager: TdxBarManager;
    bbManageConditionalFormattingRules: TdxBarButton;
    rgiColorScaleTemplates: TdxRibbonGalleryItem;
    rgiColorScaleTemplatesGroup1: TdxRibbonGalleryGroup;
    rgiColorScaleTemplatesGroup1Item1: TdxRibbonGalleryGroupItem;
    rgiColorScaleTemplatesGroup1Item2: TdxRibbonGalleryGroupItem;
    rgiColorScaleTemplatesGroup1Item3: TdxRibbonGalleryGroupItem;
    rgiColorScaleTemplatesGroup1Item4: TdxRibbonGalleryGroupItem;
    rgiColorScaleTemplatesGroup1Item5: TdxRibbonGalleryGroupItem;
    rgiColorScaleTemplatesGroup1Item6: TdxRibbonGalleryGroupItem;
    rgiColorScaleTemplatesGroup1Item12: TdxRibbonGalleryGroupItem;
    rgiColorScaleTemplatesGroup1Item7: TdxRibbonGalleryGroupItem;
    rgiColorScaleTemplatesGroup1Item8: TdxRibbonGalleryGroupItem;
    rgiColorScaleTemplatesGroup1Item9: TdxRibbonGalleryGroupItem;
    rgiColorScaleTemplatesGroup1Item10: TdxRibbonGalleryGroupItem;
    rgiColorScaleTemplatesGroup1Item11: TdxRibbonGalleryGroupItem;
    rgiDataBarTemplates: TdxRibbonGalleryItem;
    rgiDataBarTemplatesGroup1: TdxRibbonGalleryGroup;
    rgiDataBarTemplatesGroup1Item1: TdxRibbonGalleryGroupItem;
    rgiDataBarTemplatesGroup1Item2: TdxRibbonGalleryGroupItem;
    rgiDataBarTemplatesGroup1Item3: TdxRibbonGalleryGroupItem;
    rgiDataBarTemplatesGroup1Item4: TdxRibbonGalleryGroupItem;
    bbTop10: TdxBarButton;
    bbTop10Percents: TdxBarButton;
    bbBottom10: TdxBarButton;
    bbBottom10Percents: TdxBarButton;
    bbAboveAverage: TdxBarButton;
    bbBelowAverage: TdxBarButton;
    bsiTopBottomRules: TdxBarSubItem;
    rgiIconSets: TdxRibbonGalleryItem;
    rgiIconSetsGroup1: TdxRibbonGalleryGroup;
    bbClearRules: TdxBarButton;
    bbClearColumnRules: TdxBarButton;
    biHintContainer: TdxBarControlContainerItem;
    ilConditionalFormatting: TcxImageList;
    ConditionalFormattingPopupMenu: TdxBarPopupMenu;
    rgiDataBarTemplatesGroup2: TdxRibbonGalleryGroup;
    procedure bbManageConditionalFormattingRulesClick(Sender: TObject);
    procedure dxRibbonGalleryItemCreateThreeColorScaleClick(Sender: TObject);
    procedure dxRibbonGalleryItemCreateTwoColorScaleClick(Sender: TObject);
    procedure rgiDataBarTemplatesCreateDataBarRuleClick(Sender: TObject);
    procedure bbTop10Click(Sender: TObject);
    procedure bbClearRulesClick(Sender: TObject);
    procedure bbClearColumnRulesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure rgiIconSetsClick(Sender: TObject);
  end;

var
  ConditionalFormattingPopupMenuHelper: TConditionalFormattingPopupMenuHelper;

implementation

uses
  ConditionalFormattingDemoMain,
  dxSpreadSheetConditionalFormattingIconSet, dxTypeHelpers;

{$R *.dfm}

{ TConditionalFormattingPopupMenuHelper }

procedure TConditionalFormattingPopupMenuHelper.bbClearColumnRulesClick(
  Sender: TObject);
begin
  ConditionalFormattingDemoMainForm.ClearRulesFromSelectedArea;
end;

procedure TConditionalFormattingPopupMenuHelper.bbClearRulesClick(
  Sender: TObject);
begin
  ConditionalFormattingDemoMainForm.ClearRulesfromAllColumnsClick(Sender);
end;

procedure TConditionalFormattingPopupMenuHelper.bbManageConditionalFormattingRulesClick(
  Sender: TObject);
begin
  ConditionalFormattingDemoMainForm.ManageRulesClick(Sender);
end;

procedure TConditionalFormattingPopupMenuHelper.bbTop10Click(Sender: TObject);
begin
  ConditionalFormattingDemoMainForm.miTop10Click(Sender);
end;

procedure TConditionalFormattingPopupMenuHelper.dxRibbonGalleryItemCreateThreeColorScaleClick(
  Sender: TObject);
begin
  ConditionalFormattingDemoMainForm.ThreeColorScaleClick(Sender);
end;

procedure TConditionalFormattingPopupMenuHelper.dxRibbonGalleryItemCreateTwoColorScaleClick(
  Sender: TObject);
begin
  ConditionalFormattingDemoMainForm.TwoColorScaleClick(Sender);
end;

procedure TConditionalFormattingPopupMenuHelper.FormCreate(Sender: TObject);
var
  AItem: TdxRibbonGalleryGroupItem;
  APreset: TdxSpreadSheetConditionalFormattingIconSetPreset;
  I: Integer;
begin
  rgiIconSets.GalleryOptions.Images := ConditionalFormattingIconSet.PresetPreviews;
  rgiIconSetsGroup1.Items.BeginUpdate;
  try
    for I := 0 to ConditionalFormattingIconSet.Presets.Count - 1 do
    begin
      APreset := ConditionalFormattingIconSet.Presets[I];
      if APreset.IndexOf(-1) < 0 then
      begin
        AItem := rgiIconSetsGroup1.Items.Add;
        AItem.Caption := APreset.Description;
        AItem.OnClick := rgiIconSetsClick;
        AItem.ImageIndex := I;
        AItem.Tag := I;
      end;
    end;
  finally
    rgiIconSetsGroup1.Items.EndUpdate;
  end;
end;

procedure TConditionalFormattingPopupMenuHelper.rgiDataBarTemplatesCreateDataBarRuleClick(
  Sender: TObject);
begin
  ConditionalFormattingDemoMainForm.DataBarClick(Sender);
end;

procedure TConditionalFormattingPopupMenuHelper.rgiIconSetsClick(
  Sender: TObject);
begin
  ConditionalFormattingDemoMainForm.IconSetsClick(Sender);
end;

end.
