{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSpreadSheetReportDesignerActions;

{$I cxVer.Inc}

interface

uses
  Classes, Controls, Types, SysUtils, dxCore, cxGraphics, cxGeometry, dxActions, dxHashUtils, Math,
  dxSpreadSheetCore, dxSpreadSheetReportDesigner, dxSpreadSheetActions, Generics.Defaults, Generics.Collections;

type
  TdxSpreadSheetReportDesignerAccess = class(TdxSpreadSheetReportDesigner);
  TdxSpreadSheetReportSectionAccess = class(TdxSpreadSheetReportSection);


  { TdxSpreadSheetReportDesignerAction }

  TdxSpreadSheetReportDesignerAction = class(TdxSpreadSheetAction)
  strict private
    function GetDataController: TdxSpreadSheetReportDataController;
    function GetDesigner: TdxSpreadSheetReportDesignerAccess;
    function GetIsDetailSection: Boolean;
    function GetSection: TdxSpreadSheetReportSectionAccess;
  protected
    function CheckSection(ASectionType: TdxSpreadSheetReportSectionType): Boolean;
    function GetMaxSectionID(ASectionType: TdxSpreadSheetReportSectionType): Integer;
    procedure Initialize; virtual;
    procedure UpdateControl(Target: TObject); override;

    property DataController: TdxSpreadSheetReportDataController read GetDataController;
    property Designer: TdxSpreadSheetReportDesignerAccess read GetDesigner;
    property IsDetailSection: Boolean read GetIsDetailSection;
    property Section: TdxSpreadSheetReportSectionAccess read GetSection;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxSpreadSheetReportDesignerModeAction }

  TdxSpreadSheetReportDesignerModeAction = class(TdxSpreadSheetReportDesignerAction)
  protected
    FReportMode: TdxSpreadSheetReportMode;
    //
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    procedure DoResetState; override;
  end;

  { TdxSpreadSheetReportDesignerSingleSheetReportMode }

  TdxSpreadSheetReportDesignerSingleSheetReportMode = class(TdxSpreadSheetReportDesignerModeAction)
  protected
    procedure Initialize; override;
  end;

  { TdxSpreadSheetReportDesignerMultipleSheetsReportMode }

  TdxSpreadSheetReportDesignerMultipleSheetsReportMode = class(TdxSpreadSheetReportDesignerModeAction)
  protected
    procedure Initialize; override;
  end;

  { TdxSpreadSheetReportDesignerMultipleDocumentsReportMode }

  TdxSpreadSheetReportDesignerMultipleDocumentsReportMode = class(TdxSpreadSheetReportDesignerModeAction)
  protected
    procedure Initialize; override;
  end;

  { TdxSpreadSheetReportDesignerOrientationAction }

  TdxSpreadSheetReportDesignerOrientationAction = class(TdxSpreadSheetReportDesignerAction)
  protected
    FReportOrientation: TdxSpreadSheetReportOrientation;
    //
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    procedure DoResetState; override;
  published
    property AutoCheck default True;
  end;

  { TdxSpreadSheetReportDesignerHorizontalOrientation }

  TdxSpreadSheetReportDesignerHorizontalOrientation = class(TdxSpreadSheetReportDesignerOrientationAction)
  protected
    procedure Initialize; override;
  end;

  { TdxSpreadSheetReportDesignerVerticalOrientation }

  TdxSpreadSheetReportDesignerVerticalOrientation = class(TdxSpreadSheetReportDesignerOrientationAction)
  protected
    procedure Initialize; override;
  end;

  { TdxSpreadSheetReportDesignerHeaderSection }

  TdxSpreadSheetReportDesignerHeaderSection = class(TdxSpreadSheetReportDesignerAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    procedure Initialize; override;
  end;

  { TdxSpreadSheetReportDesignerFooterSection }

  TdxSpreadSheetReportDesignerFooterSection = class(TdxSpreadSheetReportDesignerAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    procedure Initialize; override;
  end;

  { TdxSpreadSheetReportDesignerDetailSection }

  TdxSpreadSheetReportDesignerDetailSection = class(TdxSpreadSheetReportDesignerAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    procedure Initialize; override;
  end;

  { TdxSpreadSheetReportDesignerDetailLevel }

  TdxSpreadSheetReportDesignerDetailLevel = class(TdxSpreadSheetReportDesignerAction)
  protected
    procedure DoExecute; override;
    procedure Initialize; override;
    function IsEnabled: Boolean; override;
  end;

  { TdxSpreadSheetReportDesignerDataMember }

  TdxSpreadSheetReportDesignerDataMember = class(TdxSpreadSheetReportDesignerAction)
  protected
    procedure Initialize; override;
    function IsEnabled: Boolean; override;
  end;

  { TdxSpreadSheetReportDesignerResetSection }

  TdxSpreadSheetReportDesignerResetSection = class(TdxSpreadSheetReportDesignerAction)
  protected
    procedure DoExecute; override;
    procedure Initialize; override;
    function IsEnabled: Boolean; override;
  end;

  { TdxSpreadSheetReportDesignerSortFields }

  TdxSpreadSheetReportDesignerSortFields = class(TdxSpreadSheetReportDesignerAction)
  protected
    procedure Initialize; override;
    function IsEnabled: Boolean; override;
  end;

  { TdxSpreadSheetReportDesignerGroupHeaderSection }

  TdxSpreadSheetReportDesignerGroupHeaderSection = class(TdxSpreadSheetReportDesignerAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    procedure Initialize; override;
    function IsEnabled: Boolean; override;
  end;

  { TdxSpreadSheetReportDesignerGroupFooterSection }

  TdxSpreadSheetReportDesignerGroupFooterSection = class(TdxSpreadSheetReportDesignerAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    procedure Initialize; override;
    function IsEnabled: Boolean; override;
  end;

  { TdxSpreadSheetReportDesignerEditFilter }

  TdxSpreadSheetReportDesignerEditFilter = class(TdxSpreadSheetReportDesignerAction)
  protected
    procedure Initialize; override;
    function IsEnabled: Boolean; override;
  end;

  { TdxSpreadSheetReportDesignerResetFilter }

  TdxSpreadSheetReportDesignerResetFilter = class(TdxSpreadSheetReportDesignerAction)
  protected
    procedure DoExecute; override;
    procedure Initialize; override;
    function IsEnabled: Boolean; override;
  end;

  { TdxSpreadSheetReportDesignerDesignView }

  TdxSpreadSheetReportDesignerDesignView = class(TdxSpreadSheetReportDesignerAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    procedure Initialize; override;
  end;

  { TdxSpreadSheetReportDesignerReportPreview }

  TdxSpreadSheetReportDesignerReportPreview = class(TdxSpreadSheetReportDesignerAction)
  protected
    procedure Initialize; override;
    function IsEnabled: Boolean; override;
  end;


resourcestring
  sdxSpreadSheetReportDesignerActionSingleSheetReportModeCaption     = 'Single Sheet';
  sdxSpreadSheetReportDesignerActionSingleSheetReportModeHint = 'Insert all sections into a single worksheet according to the selected document orientation.';

  sdxSpreadSheetReportDesignerActionMultipleSheetsReportModeCaption     = 'Multiple Sheets';
  sdxSpreadSheetReportDesignerActionMultipleSheetsReportModeHint = 'Create a separate worksheet in a single workbook for each record of the data source.';

  sdxSpreadSheetReportDesignerActionMultipleDocumentsReportModeCaption     = 'Multiple Documents';
  sdxSpreadSheetReportDesignerActionMultipleDocumentsReportModeHint = 'Create a separate workbook for each record of the data source.';

  sdxSpreadSheetReportDesignerActionHorizontalOrientationCaption     = 'Horizontal';
  sdxSpreadSheetReportDesignerActionHorizontalOrientationHint = 'Insert sections one after the other from left to right. The header is on the left, and the footer is on the right of the sheet.';

  sdxSpreadSheetReportDesignerActionVerticalOrientationCaption     = 'Vertical';
  sdxSpreadSheetReportDesignerActionVerticalOrientationHint = 'Insert sections one under the other. The header is on the top, and the footer is on the bottom of the sheet.';

  sdxSpreadSheetReportDesignerActionOrientationCommandGroupCaption     = 'Document Orientation';
  sdxSpreadSheetReportDesignerActionOrientationCommandGroupHint = 'Select either vertical or horizontal orientation for a resulting document.';

  sdxSpreadSheetReportDesignerActionHeaderSectionCaption     = 'Header';
  sdxSpreadSheetReportDesignerActionHeaderSectionHint = 'Specify a header section in the template.';

  sdxSpreadSheetReportDesignerActionFooterSectionCaption     = 'Footer';
  sdxSpreadSheetReportDesignerActionFooterSectionHint = 'Specify a footer section in the template.';

  sdxSpreadSheetReportDesignerActionDetailSectionCaption     = 'Detail';
  sdxSpreadSheetReportDesignerActionDetailSectionHint = 'Specify a detail section to repeat for each record of the data source.';

  sdxSpreadSheetReportDesignerActionDetailLevelSectionCaption     = 'Detail Level';
  sdxSpreadSheetReportDesignerActionDetailLevelSectionHint = 'Specify the detail section of the next level.';

  sdxSpreadSheetReportDesignerActionDetailDataMemberCaption     = 'Data Member';
  sdxSpreadSheetReportDesignerActionDetailDataMemberHint = 'Set a data member for a detail level.';

  sdxSpreadSheetReportDesignerActionResetSectionCaption     = 'Reset';
  sdxSpreadSheetReportDesignerActionResetSectionHint = 'Reset the selected template section.';

  sdxSpreadSheetReportDesignerActionSortFieldsCaption     = 'Sort Fields';
  sdxSpreadSheetReportDesignerActionSortFieldsHint = 'Specify sorting criteria for data in the selected detail section.';

  sdxSpreadSheetReportDesignerActionGroupHeaderCaption     = 'Group Header';
  sdxSpreadSheetReportDesignerActionGroupHeaderHint = 'Specify a group header. It is based on a sort field and displays information at the beginning of a group of records in a resulting document.';

  sdxSpreadSheetReportDesignerActionGroupFooterCaption     = 'Group Footer';
  sdxSpreadSheetReportDesignerActionGroupFooterHint = 'Specify a group footer. It is based on a sort field and displays information at the end of a group of records in a resulting document.';

  sdxSpreadSheetReportDesignerActionEditFilterCaption     = 'Edit Filter';
  sdxSpreadSheetReportDesignerActionEditFilterHint = 'Specify filter criteria for data in the selected detail section.';

  sdxSpreadSheetReportDesignerActionResetFilterCaption     = 'Reset Filter';
  sdxSpreadSheetReportDesignerActionResetFilterHint = 'Reset data filter in the selected detail section.';

  sdxSpreadSheetReportDesignerActionDesignViewCaption     = 'Design View';
  sdxSpreadSheetReportDesignerActionDesignViewHint = 'Highlight template sections.';

  sdxSpreadSheetReportDesignerActionReportPreviewCaption     = 'Report Preview';
  sdxSpreadSheetReportDesignerActionReportPreviewHint = 'Preview a resulting document.';

implementation

{ TdxSpreadSheetReportDesigner }

constructor TdxSpreadSheetReportDesignerAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Initialize;
  AutoCheck := True;
end;

function TdxSpreadSheetReportDesignerAction.CheckSection(
  ASectionType: TdxSpreadSheetReportSectionType): Boolean;
begin
  Result := (Section <> nil) and (Section.SectionType = ASectionType)
end;

function TdxSpreadSheetReportDesignerAction.GetMaxSectionID(
  ASectionType: TdxSpreadSheetReportSectionType): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Designer.SectionList.Count - 1 do
    if Designer.SectionList[I].SectionType = ASectionType then
      Result := Max(Result, TdxSpreadSheetReportSectionAccess(Designer.SectionList[I]).Index);
end;

procedure TdxSpreadSheetReportDesignerAction.Initialize;
begin
end;

procedure  TdxSpreadSheetReportDesignerAction.UpdateControl(Target: TObject);
begin
  if Target is TdxSpreadSheetReportDesigner then
    Control := TdxSpreadSheetReportDesigner(Target);
end;

function TdxSpreadSheetReportDesignerAction.GetDataController: TdxSpreadSheetReportDataController;
begin
  Result := Designer.GetDataControllerForSection(Section);
end;

function TdxSpreadSheetReportDesignerAction.GetDesigner: TdxSpreadSheetReportDesignerAccess;
begin
  Result := TdxSpreadSheetReportDesignerAccess(Control);
end;

function TdxSpreadSheetReportDesignerAction.GetIsDetailSection: Boolean;
begin
  Result := (Section <> nil) and (Section.SectionType in [rstDetail, rstDetailLevel]);
end;


function TdxSpreadSheetReportDesignerAction.GetSection: TdxSpreadSheetReportSectionAccess;
begin
  Result := TdxSpreadSheetReportSectionAccess(Designer.CurrentSection);
end;

{ TdxSpreadSheetReportDesignerModeAction }

procedure TdxSpreadSheetReportDesignerModeAction.DoExecute;
begin
  if Checked then
    Designer.Options.ReportMode := FReportMode
  else
    Designer.Options.ReportMode := rmSingleSheet;
end;

procedure TdxSpreadSheetReportDesignerModeAction.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := Designer.Options.ReportMode = FReportMode;
end;

procedure TdxSpreadSheetReportDesignerModeAction.DoResetState;
begin
  inherited DoResetState;
  Checked := False;
end;

{ TdxSpreadSheetReportDesignerSingleSheetReportMode }

procedure TdxSpreadSheetReportDesignerSingleSheetReportMode.Initialize;
begin
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionSingleSheetReportModeCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionSingleSheetReportModeHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\Mode\SingleSheet.png';
  FIsSelectionNeeded := False;
end;

{ TdxSpreadSheetReportDesignerMultipleSheets }

procedure TdxSpreadSheetReportDesignerMultipleSheetsReportMode.Initialize;
begin
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionMultipleSheetsReportModeCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionMultipleSheetsReportModeHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\Mode\MultipleSheets.png';
  FIsSelectionNeeded := False;
  FReportMode := rmMultipleSheets;
end;

{ TdxSpreadSheetReportDesignerMultipleDocuments }

procedure TdxSpreadSheetReportDesignerMultipleDocumentsReportMode.Initialize;
begin
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionMultipleDocumentsReportModeCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionMultipleDocumentsReportModeHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\Mode\MultipleDocuments.png';
  FIsSelectionNeeded := False;
  FReportMode := rmMultipleDocuments;
end;

{ TdxSpreadSheetReportDesignerOrientationAction }

procedure TdxSpreadSheetReportDesignerOrientationAction.DoExecute;
begin
  if Checked then
    Designer.Options.Orientation := FReportOrientation
  else
    Designer.Options.Orientation := roVertical;
end;

procedure TdxSpreadSheetReportDesignerOrientationAction.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := Designer.Options.Orientation = FReportOrientation;
end;

procedure TdxSpreadSheetReportDesignerOrientationAction.DoResetState;
begin
  inherited DoResetState;
  Checked := False;
end;

{ TdxSpreadSheetReportDesignerHorizontalOrientation }

procedure TdxSpreadSheetReportDesignerHorizontalOrientation.Initialize;
begin
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionHorizontalOrientationCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionHorizontalOrientationHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\Mode\HorizontalMode.png';
  FIsSelectionNeeded := False;
  FReportOrientation := roHorizontal;
end;

{ TdxSpreadSheetReportDesignerVerticalOrientation }

procedure TdxSpreadSheetReportDesignerVerticalOrientation.Initialize;
begin
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionVerticalOrientationCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionVerticalOrientationHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\Mode\VerticalMode.png';
  FIsSelectionNeeded := False;
  FReportOrientation := roVertical;
end;

{ TdxSpreadSheetReportDesignerHeaderSection }

procedure TdxSpreadSheetReportDesignerHeaderSection.DoExecute;
begin
  Designer.SetHeaderSection(Designer.ActiveSheetAsTable.Selection.Area);
end;

procedure TdxSpreadSheetReportDesignerHeaderSection.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := CheckSection(rstHeader) and (Section.Index = -1)
end;

procedure TdxSpreadSheetReportDesignerHeaderSection.Initialize;
begin
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionHeaderSectionCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionHeaderSectionHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\TemplateRanges\SetHeaderRange.png';
end;

{ TdxSpreadSheetReportDesignerFooterSection }

procedure TdxSpreadSheetReportDesignerFooterSection.Initialize;
begin
  AutoCheck := False;
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionFooterSectionCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionFooterSectionHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\TemplateRanges\SetFooterRange.png';
end;

procedure TdxSpreadSheetReportDesignerFooterSection.DoExecute;
begin
  Designer.SetFooterSection(Designer.ActiveSheetAsTable.Selection.Area);
end;

procedure TdxSpreadSheetReportDesignerFooterSection.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := CheckSection(rstFooter) and (Section.Index = -1)
end;

{ TdxSpreadSheetReportDesignerDetailSection }

procedure TdxSpreadSheetReportDesignerDetailSection.DoExecute;
begin
  Designer.SetDetailSection(Designer.ActiveSheetAsTable.Selection.Area);
end;

procedure TdxSpreadSheetReportDesignerDetailSection.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := CheckSection(rstDetail) and (Section.Index = -1)
end;

procedure TdxSpreadSheetReportDesignerDetailSection.Initialize;
begin
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionDetailSectionCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionDetailSectionHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\TemplateRanges\SetDetailRange.png';
end;

{ TdxSpreadSheetReportDesignerDetailLevel }

procedure TdxSpreadSheetReportDesignerDetailLevel.DoExecute;
begin
  Designer.SetDetailSection(Designer.ActiveSheetAsTable.Selection.Area,
    GetMaxSectionID(rstDetailLevel) + 1);
end;

procedure TdxSpreadSheetReportDesignerDetailLevel.Initialize;
begin
  AutoCheck := False;
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionDetailLevelSectionCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionDetailLevelSectionHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\TemplateRanges\SetDetailLevel.png';
end;

function TdxSpreadSheetReportDesignerDetailLevel.IsEnabled: Boolean;
begin
  Result := IsDetailSection;
end;

{ TdxSpreadSheetReportDesignerDataMember }

procedure TdxSpreadSheetReportDesignerDataMember.Initialize;
begin
  AutoCheck := False;
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionDetailDataMemberCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionDetailDataMemberHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\TemplateRanges\SetDetailDataMember.png';
end;

function TdxSpreadSheetReportDesignerDataMember.IsEnabled: Boolean;
begin
  Result := CheckSection(rstDetailLevel) and Assigned(OnExecute);
end;

{ TdxSpreadSheetReportDesignerResetSection }

procedure TdxSpreadSheetReportDesignerResetSection.DoExecute;
begin
  Designer.RemoveSection(Section);
end;

procedure TdxSpreadSheetReportDesignerResetSection.Initialize;
begin
  AutoCheck := False;
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionResetSectionCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionResetSectionHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\TemplateRanges\ResetRange.png';
end;

function TdxSpreadSheetReportDesignerResetSection.IsEnabled: Boolean;
begin
  Result := Section <> nil;
end;

{ TdxSpreadSheetReportDesignerSortFields }

procedure TdxSpreadSheetReportDesignerSortFields.Initialize;
begin
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionSortFieldsCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionSortFieldsHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\SortAndGroup\SortFields.png';
end;

function TdxSpreadSheetReportDesignerSortFields.IsEnabled: Boolean;
begin
  Result := DataController <> nil;
end;

{ TdxSpreadSheetReportDesignerGroupHeader }

procedure TdxSpreadSheetReportDesignerGroupHeaderSection.DoExecute;
begin
  if CheckSection(rstGroupHeader) then
    Designer.SetGroupHeaderSection(
      Designer.ActiveSheetAsTable.Selection.Area, Section.Index)
  else
    Designer.SetGroupHeaderSection(
      Designer.ActiveSheetAsTable.Selection.Area, GetMaxSectionID(rstGroupHeader) + 1);
end;

procedure TdxSpreadSheetReportDesignerGroupHeaderSection.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := CheckSection(rstGroupHeader);
end;

procedure TdxSpreadSheetReportDesignerGroupHeaderSection.Initialize;
begin
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionGroupHeaderCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionGroupHeaderHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\SortAndGroup\GroupHeader.png';
end;

function TdxSpreadSheetReportDesignerGroupHeaderSection.IsEnabled: Boolean;
begin
  Result := IsDetailSection or CheckSection(rstGroupHeader);
end;

{ TdxSpreadSheetReportDesignerGroupFooterSection }

procedure TdxSpreadSheetReportDesignerGroupFooterSection.DoExecute;
begin
  if CheckSection(rstGroupFooter) then
    Designer.SetGroupFooterSection(
      Designer.ActiveSheetAsTable.Selection.Area, Section.Index)
  else
    Designer.SetGroupFooterSection(
      Designer.ActiveSheetAsTable.Selection.Area, GetMaxSectionID(rstGroupFooter) + 1);
end;

procedure TdxSpreadSheetReportDesignerGroupFooterSection.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := CheckSection(rstGroupFooter);
end;

procedure TdxSpreadSheetReportDesignerGroupFooterSection.Initialize;
begin
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionGroupFooterCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionGroupFooterHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\SortAndGroup\GroupFooter.png';
end;

function TdxSpreadSheetReportDesignerGroupFooterSection.IsEnabled: Boolean;
begin
  Result := IsDetailSection or CheckSection(rstGroupFooter);
end;

{ TdxSpreadSheetReportDesignerEditFilter }

procedure TdxSpreadSheetReportDesignerEditFilter.Initialize;
begin
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionEditFilterCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionEditFilterHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\Filter\EditFilter.png';
end;

function TdxSpreadSheetReportDesignerEditFilter.IsEnabled: Boolean;
begin
  Result := (DataController <> nil) and Assigned(OnExecute);
end;

{ TdxSpreadSheetReportDesignerResetFilter }

procedure TdxSpreadSheetReportDesignerResetFilter.DoExecute;
begin
  DataController.Filter.Active := False;
  DataController.Filter.Clear;
end;

procedure TdxSpreadSheetReportDesignerResetFilter.Initialize;
begin
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionResetFilterCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionResetFilterHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\Filter\ResetFilter.png';
end;

function TdxSpreadSheetReportDesignerResetFilter.IsEnabled: Boolean;
begin
  Result := (DataController <> nil) and (DataController.Filter.FilterText <> '');
end;

{ TdxSpreadSheetReportDesignerDesignView }

procedure TdxSpreadSheetReportDesignerDesignView.DoExecute;
begin
  Designer.Options.DesignView := Checked;
end;

procedure TdxSpreadSheetReportDesignerDesignView.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := Designer.Options.DesignView;
end;

procedure TdxSpreadSheetReportDesignerDesignView.Initialize;
begin
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionDesignViewCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionDesignViewHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\Design\ShowRanges.png';
  FIsSelectionNeeded := False;
end;

{ TdxSpreadSheetReportDesignerPreview }

procedure TdxSpreadSheetReportDesignerReportPreview.Initialize;
begin
  FDefaultCaptionResString := @sdxSpreadSheetReportDesignerActionReportPreviewCaption;
  FDefaultHintResString := @sdxSpreadSheetReportDesignerActionReportPreviewHint;
  FDefaultImageNameInIconLibrary := 'Report Designer\Design\ReportPreview.png';
  FIsSelectionNeeded := False;
end;

procedure AddSpreadSheetDesignerResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxSpreadSheetReportDesignerActionSingleSheetReportModeCaption', @sdxSpreadSheetReportDesignerActionSingleSheetReportModeCaption);;
  AProduct.Add('sdxSpreadSheetReportDesignerActionSingleSheetReportModeHint', @sdxSpreadSheetReportDesignerActionSingleSheetReportModeHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionMultipleSheetsReportModeCaption', @sdxSpreadSheetReportDesignerActionMultipleSheetsReportModeCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionMultipleSheetsReportModeHint', @sdxSpreadSheetReportDesignerActionMultipleSheetsReportModeHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionMultipleDocumentsReportModeCaption', @sdxSpreadSheetReportDesignerActionMultipleDocumentsReportModeCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionMultipleDocumentsReportModeHint', @sdxSpreadSheetReportDesignerActionMultipleDocumentsReportModeHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionHorizontalOrientationCaption', @sdxSpreadSheetReportDesignerActionHorizontalOrientationCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionHorizontalOrientationHint', @sdxSpreadSheetReportDesignerActionHorizontalOrientationHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionVerticalOrientationCaption', @sdxSpreadSheetReportDesignerActionVerticalOrientationCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionVerticalOrientationHint', @sdxSpreadSheetReportDesignerActionVerticalOrientationHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionOrientationCommandGroupCaption', @sdxSpreadSheetReportDesignerActionOrientationCommandGroupCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionOrientationCommandGroupHint', @sdxSpreadSheetReportDesignerActionOrientationCommandGroupHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionHeaderSectionCaption', @sdxSpreadSheetReportDesignerActionHeaderSectionCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionHeaderSectionHint', @sdxSpreadSheetReportDesignerActionHeaderSectionHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionFooterSectionCaption', @sdxSpreadSheetReportDesignerActionFooterSectionCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionFooterSectionHint', @sdxSpreadSheetReportDesignerActionFooterSectionHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionDetailSectionCaption', @sdxSpreadSheetReportDesignerActionDetailSectionCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionDetailSectionHint', @sdxSpreadSheetReportDesignerActionDetailSectionHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionDetailLevelSectionCaption', @sdxSpreadSheetReportDesignerActionDetailLevelSectionCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionDetailLevelSectionHint', @sdxSpreadSheetReportDesignerActionDetailLevelSectionHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionDetailDataMemberCaption', @sdxSpreadSheetReportDesignerActionDetailDataMemberCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionDetailDataMemberHint', @sdxSpreadSheetReportDesignerActionDetailDataMemberHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionResetSectionCaption', @sdxSpreadSheetReportDesignerActionResetSectionCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionResetSectionHint', @sdxSpreadSheetReportDesignerActionResetSectionHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionSortFieldsCaption', @sdxSpreadSheetReportDesignerActionSortFieldsCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionSortFieldsHint', @sdxSpreadSheetReportDesignerActionSortFieldsHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionGroupHeaderCaption', @sdxSpreadSheetReportDesignerActionGroupHeaderCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionGroupHeaderHint', @sdxSpreadSheetReportDesignerActionGroupHeaderHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionGroupFooterCaption', @sdxSpreadSheetReportDesignerActionGroupFooterCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionGroupFooterHint', @sdxSpreadSheetReportDesignerActionGroupFooterHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionEditFilterCaption', @sdxSpreadSheetReportDesignerActionEditFilterCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionEditFilterHint', @sdxSpreadSheetReportDesignerActionEditFilterHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionResetFilterCaption', @sdxSpreadSheetReportDesignerActionResetFilterCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionResetFilterHint', @sdxSpreadSheetReportDesignerActionResetFilterHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionDesignViewCaption', @sdxSpreadSheetReportDesignerActionDesignViewCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionDesignViewHint', @sdxSpreadSheetReportDesignerActionDesignViewHint);
  AProduct.Add('sdxSpreadSheetReportDesignerActionReportPreviewCaption', @sdxSpreadSheetReportDesignerActionReportPreviewCaption);
  AProduct.Add('sdxSpreadSheetReportDesignerActionReportPreviewHint', @sdxSpreadSheetReportDesignerActionReportPreviewHint);
end;


function TdxSpreadSheetReportDesignerReportPreview.IsEnabled: Boolean;
begin
  Result := Designer.DataBinding.DataController.Active and Assigned(OnExecute);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressSpreadSheet 2', @AddSpreadSheetDesignerResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressSpreadSheet 2', @AddSpreadSheetDesignerResourceStringNames);

end.
