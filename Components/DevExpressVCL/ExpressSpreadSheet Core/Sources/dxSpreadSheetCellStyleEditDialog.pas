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

unit dxSpreadSheetCellStyleEditDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
{$IFDEF DELPHI101BERLIN}
  System.ImageList,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls,
  Generics.Defaults, Generics.Collections, ExtCtrls, ImgList,
  cxPC, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxButtons, dxCore, cxImageList, dxForms,
  dxLayoutLookAndFeels, dxLayoutContainer, dxLayoutControl, cxContainer, cxEdit, cxListBox, dxLayoutControlAdapters,
  dxLayoutcxEditAdapters, cxLabel, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxSpinEdit, cxCheckBox, cxClasses,
  cxFontNameComboBox, cxColorComboBox, cxCheckListBox, dxGalleryControl, dxColorGallery, cxImageComboBox, dxColorDialog,
  dxSpreadSheetCoreStrs, dxSpreadSheetGraphics, dxSpreadSheetClasses, dxSpreadSheetTypes, dxSpreadSheetNumberFormat,
  dxSpreadSheetStyles, dxSpreadSheetCellStyleEditDialogController, dxSpreadSheetCellStyleEditDialogHelpers,
  dxSpreadSheetCoreStyles;

type

  { TdxSpreadSheetCellStyleEditDialogForm }

  TdxSpreadSheetCellStyleEditDialogForm = class(TdxForm)
    btnBorderBottom: TcxButton;
    btnBorderHorz: TcxButton;
    btnBorderLeft: TcxButton;
    btnBorderRight: TcxButton;
    btnBordersInside: TcxButton;
    btnBordersNone: TcxButton;
    btnBordersOutline: TcxButton;
    btnBorderTop: TcxButton;
    btnBorderVert: TcxButton;
    btnCancel: TcxButton;
    btnFillMoreColors: TcxButton;
    btnFillNoColor: TcxButton;
    btnFontReset: TcxButton;
    btnOK: TcxButton;
    cbFontColor: TcxColorComboBox;
    cbFontStrikethrough: TcxCheckBox;
    cbFontUnderline: TcxComboBox;
    cbHidden: TcxCheckBox;
    cbLocked: TcxCheckBox;
    cbMergeCells: TcxCheckBox;
    cbShrinkToFit: TcxCheckBox;
    cbUseThousandSeparator: TcxCheckBox;
    cbWrapText: TcxCheckBox;
    ccbBorderLineColor: TcxColorComboBox;
    ccbPatternColor: TcxColorComboBox;
    ccbTextAlignHorz: TcxComboBox;
    ccbTextAlignVert: TcxComboBox;
    cgFill: TdxColorGallery;
    ColorDialog: TdxColorDialog;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    icbPatternStyle: TcxImageComboBox;
    ilFillPatterns: TcxImageList;
    lbBordersHint: TcxLabel;
    lbCategory: TcxListBox;
    lbCategoryDescription: TcxLabel;
    lbFontNames: TcxListBox;
    lbFontSizes: TcxListBox;
    lbFontStyles: TcxListBox;
    lbFontWarning: TcxLabel;
    lbGeneralNotes: TcxLabel;
    lbLineStyles: TcxListBox;
    lbNumberFormatTemplates: TcxListBox;
    lbProtectionNotes: TcxLabel;
    lbSample: TcxLabel;
    lcgBorders: TdxLayoutGroup;
    lcgFill: TdxLayoutGroup;
    lcgFillSample: TdxLayoutGroup;
    lcgFont: TdxLayoutGroup;
    lcgFontEffects: TdxLayoutGroup;
    lcgFontPreview: TdxLayoutGroup;
    lcgLine: TdxLayoutGroup;
    lcgNumber: TdxLayoutGroup;
    lcgNumberSample: TdxLayoutGroup;
    lcgProtection: TdxLayoutGroup;
    lcgTextAlignment: TdxLayoutGroup;
    lciBorderInside: TdxLayoutLabeledItem;
    lciBorderNone: TdxLayoutLabeledItem;
    lciBorderOutline: TdxLayoutLabeledItem;
    lciCategory: TdxLayoutItem;
    lciCustomFormatCode: TdxLayoutItem;
    lciDecimalPlaces: TdxLayoutItem;
    lciFillBackgroundColor: TdxLayoutLabeledItem;
    lciFillPatternColor: TdxLayoutItem;
    lciFillPatternStyle: TdxLayoutItem;
    lciFont: TdxLayoutItem;
    lciFontColor: TdxLayoutItem;
    lciFontSize: TdxLayoutItem;
    lciFontStyle: TdxLayoutItem;
    lciFontUnderline: TdxLayoutItem;
    lciGeneralNotes: TdxLayoutItem;
    lciLineColor: TdxLayoutItem;
    lciLineStyles: TdxLayoutItem;
    lciNumberFormatTemplates: TdxLayoutItem;
    lciReset: TdxLayoutItem;
    lciSample: TdxLayoutItem;
    lciShrinkToFit: TdxLayoutItem;
    lciTextAlignHorz: TdxLayoutItem;
    lciTextAlignHorzIndent: TdxLayoutItem;
    lciTextAlignVert: TdxLayoutItem;
    lciUseThousandSeparator: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMainGroup1: TdxLayoutGroup;
    lcMainGroup10: TdxLayoutAutoCreatedGroup;
    lcMainGroup11: TdxLayoutAutoCreatedGroup;
    lcMainGroup12: TdxLayoutGroup;
    lcMainGroup13: TdxLayoutAutoCreatedGroup;
    lcMainGroup14: TdxLayoutGroup;
    lcMainGroup15: TdxLayoutAutoCreatedGroup;
    lcMainGroup16: TdxLayoutGroup;
    lcMainGroup17: TdxLayoutGroup;
    lcMainGroup18: TdxLayoutAutoCreatedGroup;
    lcMainGroup19: TdxLayoutGroup;
    lcMainGroup2: TdxLayoutGroup;
    lcMainGroup20: TdxLayoutGroup;
    lcMainGroup21: TdxLayoutGroup;
    lcMainGroup22: TdxLayoutAutoCreatedGroup;
    lcMainGroup23: TdxLayoutAutoCreatedGroup;
    lcMainGroup24: TdxLayoutAutoCreatedGroup;
    lcMainGroup25: TdxLayoutAutoCreatedGroup;
    lcMainGroup26: TdxLayoutAutoCreatedGroup;
    lcMainGroup27: TdxLayoutGroup;
    lcMainGroup28: TdxLayoutAutoCreatedGroup;
    lcMainGroup29: TdxLayoutGroup;
    lcMainGroup3: TdxLayoutGroup;
    lcMainGroup30: TdxLayoutGroup;
    lcMainGroup4: TdxLayoutAutoCreatedGroup;
    lcMainGroup5: TdxLayoutAutoCreatedGroup;
    lcMainGroup6: TdxLayoutGroup;
    lcMainContentGroup: TdxLayoutGroup;
    lcMainGroup8: TdxLayoutGroup;
    lcMainGroup9: TdxLayoutAutoCreatedGroup;
    lcMainItem1: TdxLayoutItem;
    lciMergeCells: TdxLayoutItem;
    lcMainItem12: TdxLayoutItem;
    lcMainItem13: TdxLayoutItem;
    lcMainItem14: TdxLayoutItem;
    lcMainItem15: TdxLayoutItem;
    lcMainItem16: TdxLayoutItem;
    lcMainItem17: TdxLayoutItem;
    lcMainItem18: TdxLayoutItem;
    lcMainItem19: TdxLayoutItem;
    lcMainItem2: TdxLayoutItem;
    lcMainItem20: TdxLayoutItem;
    lcMainItem23: TdxLayoutItem;
    lcMainItem24: TdxLayoutItem;
    lcMainItem25: TdxLayoutItem;
    lcMainItem26: TdxLayoutItem;
    lcMainItem27: TdxLayoutItem;
    lcMainItem28: TdxLayoutItem;
    lcMainItem29: TdxLayoutItem;
    lcMainItem3: TdxLayoutItem;
    lcMainItem30: TdxLayoutItem;
    lcMainItem31: TdxLayoutItem;
    lcMainItem32: TdxLayoutItem;
    lcMainItem33: TdxLayoutItem;
    lcMainItem34: TdxLayoutItem;
    lcMainItem5: TdxLayoutItem;
    lcMainItem6: TdxLayoutItem;
    lcMainItem7: TdxLayoutItem;
    lcMainItem9: TdxLayoutItem;
    lcMainSpaceItem1: TdxLayoutEmptySpaceItem;
    lcMainSpaceItem2: TdxLayoutEmptySpaceItem;
    lcMainSpaceItem3: TdxLayoutEmptySpaceItem;
    pbBordersPreview: TPaintBox;
    pbFillPreview: TPaintBox;
    pbTextPreview: TPaintBox;
    seDecimalPlaces: TcxSpinEdit;
    seTextAlignHorzIndent: TcxSpinEdit;
    teCustomFormatCode: TcxTextEdit;
    teFontName: TcxTextEdit;
    teFontSize: TcxMaskEdit;
    teFontStyle: TcxTextEdit;
    lsiTextAlignment: TdxLayoutSeparatorItem;
    lsiTextControl: TdxLayoutSeparatorItem;
    lsiPresets: TdxLayoutSeparatorItem;
    lsiBorder: TdxLayoutSeparatorItem;

    procedure btnBorderClick(Sender: TObject);
    procedure btnBordersInsideClick(Sender: TObject);
    procedure btnBordersNoneClick(Sender: TObject);
    procedure btnBordersOutlineClick(Sender: TObject);
    procedure btnFillMoreColorsClick(Sender: TObject);
    procedure btnFillNoColorClick(Sender: TObject);
    procedure btnFontResetClick(Sender: TObject);
    procedure cbColorComboBoxButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cbColorComboBoxNamingConvention(Sender: TObject; const AColor: TColor; var AColorDescription: string);
    procedure cbFillChanged(Sender: TObject);
    procedure cbFontChanged(Sender: TObject);
    procedure cbWrapTextPropertiesChange(Sender: TObject);
    procedure ccbBorderLineColorPropertiesChange(Sender: TObject);
    procedure ccbTextAlignHorzPropertiesChange(Sender: TObject);
    procedure cgFillItemClick(Sender: TObject; AItem: TdxGalleryControlItem);
    procedure clbFontStylesClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
    procedure lbCategoryClick(Sender: TObject);
    procedure lbFontNamesClick(Sender: TObject);
    procedure lbFontSizesClick(Sender: TObject);
    procedure lbFontStylesClick(Sender: TObject);
    procedure lbLineStylesDrawItem(AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
    procedure lbNumberFormatTemplatesClick(Sender: TObject);
    procedure NumberFormatChanged(Sender: TObject);
    procedure pbBordersPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbBordersPreviewPaint(Sender: TObject);
    procedure pbFillPreviewPaint(Sender: TObject);
    procedure pbTextPreviewPaint(Sender: TObject);
    procedure teFontNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure teFontNamePropertiesChange(Sender: TObject);
    procedure teFontSizeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure teFontSizePropertiesChange(Sender: TObject);
    procedure teFontStyleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure teFontStylePropertiesChange(Sender: TObject);
  strict private
    FBordersButtons: array [TdxSpreadSheetCellStyleEditDialogBorder] of TcxButton;
    FBordersPreview: TdxSpreadSheetCellStyleEditDialogBordersPreview;
    FController: TdxSpreadSheetCellStyleEditDialogCustomController;
    FFontLoader: TcxFontLoader;
    FHelper: TdxSpreadSheetCellStyleEditDialogHelper;
    FLoading: Boolean;
    FNumberFormat: string;

    function GetNumberFormatCategory: TdxSpreadSheetNumberFormatCategory;
    procedure BordersPreviewChanged(Sender: TObject);
    procedure FontLoaderCompleted(Sender: TObject);
    procedure FontLoaderDestroyed(Sender: TObject);
    procedure SetNumberFormat(const AValue: string);
    procedure SetNumberFormatCategory(const AValue: TdxSpreadSheetNumberFormatCategory);
  protected
    procedure ApplyLocalization;
    procedure SetBordersStyle(ABorders: TdxSpreadSheetCellStyleEditDialogBorders; AStyle: TdxSpreadSheetCellBorderStyle);
    procedure SetupControls;
    procedure SetupDefaultsForBorderLineControls;
    procedure SetupPages;
    procedure UpdateBordersButtons;
    procedure UpdateFillSettings;
    procedure UpdateFontNameWarning;
    procedure UpdateNumberFormat;
    procedure UpdateNumberFormatCategoryDescription;
    procedure UpdateNumberFormatPreview;
    procedure UpdateShrinkToFitState;

    procedure Load(ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect); overload;
    procedure LoadBorders(ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect);
    procedure LoadBrush(ABrush: TdxSpreadSheetCellBrush);
    procedure LoadFont(AFont: TdxSpreadSheetCellFont); overload;
    procedure LoadFont(AFont: TdxSpreadSheetFontHandle); overload;
    procedure LoadNumberFormat(ADataFormat: TdxSpreadSheetCellDataFormat);
    procedure LoadProtection(ACellStyle: TdxSpreadSheetCellStyle);
    procedure LoadTextAlignment(ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer);

    procedure Merge(ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect);
    procedure MergeBorders(ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect);
    procedure MergeBrush(ABrush: TdxSpreadSheetCellBrush);
    procedure MergeEditValue(AEdit: TcxCustomEdit; const AValue: Variant);
    procedure MergeFont(AFont: TdxSpreadSheetCellFont);
    procedure MergeNumberFormat(ADataFormat: TdxSpreadSheetCellDataFormat);
    procedure MergeProtection(ACellStyle: TdxSpreadSheetCellStyle);
    procedure MergeTextAlignment(ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer);

    procedure Save(ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect); overload;
    procedure SaveBorders(ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect);
    procedure SaveBrush(ABrush: TdxSpreadSheetCellBrush);
    procedure SaveFont(AFont: TdxSpreadSheetCellFont);
    procedure SaveNumberFormat(ADataFormat: TdxSpreadSheetCellDataFormat);
    procedure SaveProtection(ACellStyle: TdxSpreadSheetCellStyle);
    procedure SaveTextAlignment(ACellStyle: TdxSpreadSheetCellStyle);

    property BordersPreview: TdxSpreadSheetCellStyleEditDialogBordersPreview read FBordersPreview;
    property Controller: TdxSpreadSheetCellStyleEditDialogCustomController read FController;
    property Helper: TdxSpreadSheetCellStyleEditDialogHelper read FHelper;
    property NumberFormat: string read FNumberFormat write SetNumberFormat;
    property NumberFormatCategory: TdxSpreadSheetNumberFormatCategory read GetNumberFormatCategory write SetNumberFormatCategory;
  public
    constructor Create(AOwner: IdxDialogOwner; AActivePage: Integer = -1; ADummy: Integer = 0); reintroduce; virtual;
    destructor Destroy; override;
    procedure Initialize(AController: TdxSpreadSheetCellStyleEditDialogCustomController);
    procedure Load; overload;
    procedure Save; overload;
  end;

implementation

uses
  Math, dxTypeHelpers, dxHashUtils, cxDrawTextUtils, dxCoreGraphics, cxGeometry, dxSpreadSheetUtils,
  dxSpreadSheetCoreDialogsStrs, StrUtils, cxFormats, dxDPIAwareUtils;

{$R *.dfm}

type
  TdxColorGalleryAccess = class(TdxColorGallery);
  TdxSpreadSheetCellFontAccess = class(TdxSpreadSheetCellFont);

var
  FActivePage: Integer = 0;
  FCustomColors: TStringList;

{ TdxSpreadSheetCellStyleEditDialogForm }

constructor TdxSpreadSheetCellStyleEditDialogForm.Create(
  AOwner: IdxDialogOwner; AActivePage: Integer = -1; ADummy: Integer = 0);
begin
  inherited Create(AOwner.GetParentForm);
  if FCustomColors = nil then
    FCustomColors := TStringList.Create;
  ColorDialog.CustomColors.Assign(FCustomColors);

  FBordersPreview := TdxSpreadSheetCellStyleEditDialogBordersPreview.Create;
  FBordersPreview.OnChanged := BordersPreviewChanged;
  SetupControls;

  FFontLoader := TcxFontLoader.Create([cxftTTF]);
  FFontLoader.OnCompleteThread := FontLoaderCompleted;
  FFontLoader.OnDestroyThread := FontLoaderDestroyed;
  FFontLoader.Start;

  ApplyLocalization;

  AutoSize := False;
  lcMain.Height := lcMain.ViewInfo.ContentHeight;
  lcMain.Width := lcMain.ViewInfo.ContentWidth;
  AutoSize := True;

  if AActivePage < 0 then
    AActivePage := FActivePage;
  lcMainContentGroup.ItemIndex := AActivePage;
  SetControlLookAndFeel(Self, AOwner.GetLookAndFeel);
end;

destructor TdxSpreadSheetCellStyleEditDialogForm.Destroy;
begin
  FActivePage := lcMainContentGroup.ItemIndex;
  FCustomColors.Assign(ColorDialog.CustomColors);
  if FFontLoader <> nil then
  begin
    FFontLoader.OnCompleteThread := nil;
    FFontLoader.OnDestroyThread := nil;
    FreeAndNil(FFontLoader);
  end;
  FreeAndNil(FBordersPreview);
  FreeAndNil(FController);
  FreeAndNil(FHelper);
  inherited Destroy;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.Initialize(AController: TdxSpreadSheetCellStyleEditDialogCustomController);
begin
  FController := AController;
  FHelper := TdxSpreadSheetCellStyleEditDialogHelper.Create(Controller);
  lciMergeCells.Enabled := Controller.CanMergeAreas;
  SetupPages;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.Load;
var
  AIsFirstCall: Boolean;
begin
  FLoading := True;
  try
    BordersPreview.HasHorzInsideBorder := Controller.HasMultipleCellsAreaAtVert;
    BordersPreview.HasVertInsideBorder := Controller.HasMultipleCellsAreaAtHorz;
    btnBorderHorz.Enabled := BordersPreview.HasHorzInsideBorder;
    btnBorderVert.Enabled := BordersPreview.HasVertInsideBorder;
    btnBordersInside.Enabled := btnBorderHorz.Enabled or btnBorderVert.Enabled;

    AIsFirstCall := True;
    Controller.EnumCellStyles(
      procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
      begin
        if AIsFirstCall then
          Load(ACellStyle, ARow, AColumn, AArea)
        else
          Merge(ACellStyle, ARow, AColumn, AArea);

        AIsFirstCall := False;
      end, True);

    cbMergeCells.Enabled := Controller.HasMultipleCellsAreaAtHorz or Controller.HasMultipleCellsAreaAtVert;
    cbMergeCells.State := Controller.GetMergeAreasInfo;
    SetupDefaultsForBorderLineControls;
    UpdateNumberFormatPreview;
  finally
    FLoading := False;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.Save;
begin
  Controller.BeginSaving;
  try
    Helper.ClearCache;
    Controller.PrepareToSave;
    Controller.EnumCellStyles(
      procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
      begin
        Save(ACellStyle, ARow, AColumn, AArea);
      end, False);
    case cbMergeCells.State of
      cbsChecked:
        Controller.MergeAreas;
      cbsUnchecked:
        Controller.UnmergeAreas;
    end;
  finally
    Controller.EndSaving;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.ApplyLocalization;
var
  AFillStyle: TdxSpreadSheetCellFillStyle;
begin
  // Common
  Caption := cxGetResourceString(@sdxFormatCellsDialogCaption);
  btnCancel.Caption := cxGetResourceString(@sdxFormatCellsDialogButtonCancel);
  btnOK.Caption := cxGetResourceString(@sdxFormatCellsDialogButtonOK);

  // Number page
  lcgNumber.Caption := cxGetResourceString(@sdxFormatCellsDialogGroupNumber);
  lcgNumberSample.Caption := cxGetResourceString(@sdxFormatCellsDialogSample);
  lciCategory.Caption := cxGetResourceString(@sdxFormatCellsDialogCategory);
  lciCustomFormatCode.Caption := cxGetResourceString(@sdxFormatCellsDialogCustomCode);
  lciDecimalPlaces.Caption := cxGetResourceString(@sdxFormatCellsDialogDecimalPlaces);
  lciNumberFormatTemplates.Caption := cxGetResourceString(@sdxFormatCellsDialogNumberFormatTemplates);
  cbUseThousandSeparator.Caption := Format(cxGetResourceString(@sdxFormatCellsDialogUseThousandSeparator), [dxFormatSettings.ThousandSeparator]);
  Helper.PopulateNumberFormatCategories(lbCategory);
  UpdateNumberFormatCategoryDescription;

  // Text align page
  lcgTextAlignment.Caption := cxGetResourceString(@sdxFormatCellsDialogGroupTextAlignment);
  cbMergeCells.Caption := cxGetResourceString(@sdxFormatCellsDialogMergeCells);
  cbShrinkToFit.Caption := cxGetResourceString(@sdxFormatCellsDialogShrinkToFit);
  cbWrapText.Caption := cxGetResourceString(@sdxFormatCellsDialogWrapText);
  lsiTextAlignment.Caption := cxGetResourceString(@sdxFormatCellsDialogTextAlignment);
  lsiTextControl.Caption := cxGetResourceString(@sdxFormatCellsDialogTextControl);
  lciTextAlignHorz.Caption := cxGetResourceString(@sdxFormatCellsDialogTextAlignHorz);
  lciTextAlignHorzIndent.Caption := cxGetResourceString(@sdxFormatCellsDialogTextAlignHorzIndent);
  lciTextAlignVert.Caption := cxGetResourceString(@sdxFormatCellsDialogTextAlignVert);
  Helper.PopulateFontSizes(lbFontSizes);
  Helper.PopulateFontStyles(lbFontStyles);
  Helper.PopulateTextAlignHorz(ccbTextAlignHorz);
  Helper.PopulateTextAlignVert(ccbTextAlignVert);

  // Font page
  lcgFont.Caption := cxGetResourceString(@sdxFormatCellsDialogFont);
  btnFontReset.Caption := cxGetResourceString(@sdxFormatCellsDialogButtonResetFont);
  lcgFontEffects.Caption := cxGetResourceString(@sdxFormatCellsDialogGroupFontEffects);
  lcgFontPreview.Caption := cxGetResourceString(@sdxFormatCellsDialogFontPreview);
  lciFont.Caption := cxGetResourceString(@sdxFormatCellsDialogFontName);
  lciFontColor.Caption := cxGetResourceString(@sdxFormatCellsDialogFontColor);
  lciFontSize.Caption := cxGetResourceString(@sdxFormatCellsDialogFontSize);
  lciFontStyle.Caption := cxGetResourceString(@sdxFormatCellsDialogFontStyle);
  lciFontUnderline.Caption := cxGetResourceString(@sdxFormatCellsDialogFontUnderline);
  cbFontStrikethrough.Caption := cxGetResourceString(@sdxFormatCellsDialogFontStrikethrough);
  Helper.PopulateFontUnderlineStyles(cbFontUnderline);
  UpdateFontNameWarning;

  // Borders page
  lsiBorder.Caption := cxGetResourceString(@sdxFormatCellsDialogBorder);
  lbBordersHint.Caption := cxGetResourceString(@sdxFormatCellsDialogBordersHint);
  lsiPresets.Caption := cxGetResourceString(@sdxFormatCellsDialogBorderPresets);
  lcgBorders.Caption := cxGetResourceString(@sdxFormatCellsDialogBorder);
  lcgLine.Caption := cxGetResourceString(@sdxFormatCellsDialogBorderLine);
  lciBorderInside.Caption := cxGetResourceString(@sdxFormatCellsDialogBorderInside);
  lciBorderNone.Caption := cxGetResourceString(@sdxFormatCellsDialogBorderNone);
  lciBorderOutline.Caption := cxGetResourceString(@sdxFormatCellsDialogBorderOutline);
  lciLineColor.Caption := cxGetResourceString(@sdxFormatCellsDialogBorderLineColor);
  lciLineStyles.Caption := cxGetResourceString(@sdxFormatCellsDialogBorderLineStyle);
  BordersPreview.PreviewText := cxGetResourceString(@sdxFormatCellsDialogPreviewText);

  // Fill page
  lcgFill.Caption := cxGetResourceString(@sdxFormatCellsDialogFill);
  lcgFillSample.Caption := cxGetResourceString(@sdxFormatCellsDialogFillSample);
  lciFillBackgroundColor.Caption := cxGetResourceString(@sdxFormatCellsDialogBackgroundColor);
  btnFillNoColor.Caption := cxGetResourceString(@sdxFormatCellsDialogNoColor);
  btnFillMoreColors.Caption := cxGetResourceString(@sdxFormatCellsDialogMoreColors);
  lciFillPatternColor.Caption := cxGetResourceString(@sdxFormatCellsDialogPatternColor);
  lciFillPatternStyle.Caption := cxGetResourceString(@sdxFormatCellsDialogPatternStyle);
  for AFillStyle := Low(AFillStyle) to High(AFillStyle) do
    icbPatternStyle.Properties.Items[Ord(AFillStyle)].Description := cxGetResourceString(dxCellFillStyleNames[AFillStyle]);

  // Protection page
  cbHidden.Caption := cxGetResourceString(@sdxFormatCellsDialogHidden);
  cbLocked.Caption := cxGetResourceString(@sdxFormatCellsDialogLocked);
  lbProtectionNotes.Caption := cxGetResourceString(@sdxFormatCellsDialogProtectionNotes);
  lcgProtection.Caption := cxGetResourceString(@sdxFormatCellsDialogProtection);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.SetBordersStyle(
  ABorders: TdxSpreadSheetCellStyleEditDialogBorders; AStyle: TdxSpreadSheetCellBorderStyle);
var
  ABorder: TdxSpreadSheetCellStyleEditDialogBorder;
begin
  for ABorder := Low(ABorder) to High(ABorder) do
    if ABorder in ABorders then
    begin
      BordersPreview.BordersInfo[ABorder].Assigned := True;
      BordersPreview.BordersInfo[ABorder].Undefined := False;
      BordersPreview.BordersInfo[ABorder].Color := Helper.GetActualColor(ccbBorderLineColor);
      BordersPreview.BordersInfo[ABorder].Style := AStyle;
    end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.SetupControls;

  procedure AddAutoButton(AColorComboBox: TcxColorComboBox);
  var
    AItem: TcxEditButton;
  begin
    AItem := AColorComboBox.Properties.Buttons.Add;
    AItem.Kind := bkText;
    AItem.Caption := cxGetResourceString(@sdxFormatCellsDialogButtonColorAuto);
    AItem.Width := cxTextWidth(AColorComboBox.Style.GetVisibleFont, AItem.Caption) + 8 * cxTextSpace;
    AColorComboBox.Properties.OnButtonClick := cbColorComboBoxButtonClick;
  end;

var
  AIndex: TdxSpreadSheetCellStyleEditDialogBorder;
  I: Integer;
begin
  AddAutoButton(cbFontColor);
  AddAutoButton(ccbBorderLineColor);
  AddAutoButton(ccbPatternColor);
  for I := 0 to TdxColorGalleryAccess(cgFill).Gallery.Groups.Count - 1 do
    TdxColorGalleryAccess(cgFill).Gallery.Groups[I].ShowCaption := False;
  Helper.PopulateFillPatterns(ilFillPatterns, icbPatternStyle);

  FBordersButtons[fcdbBottom] := btnBorderBottom;
  FBordersButtons[fcdbInsideHorizontal] := btnBorderHorz;
  FBordersButtons[fcdbInsideVertical] := btnBorderVert;
  FBordersButtons[fcdbLeft] := btnBorderLeft;
  FBordersButtons[fcdbRight] := btnBorderRight;
  FBordersButtons[fcdbTop] := btnBorderTop;
  for AIndex := Low(AIndex) to High(AIndex) do
    FBordersButtons[AIndex].Tag := Ord(AIndex);
  for I := Ord(sscbsDefault) to Ord(sscbsNone) - 1 do
    lbLineStyles.AddItem('', TObject(I));
  lbLineStyles.ItemIndex := Ord(sscbsThin);
  lbLineStyles.ItemHeight := cxTextHeight(lbLineStyles.Style.Font) + 2 * cxTextSpace;
  pbFillPreview.Tag := clDefault;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.SetupDefaultsForBorderLineControls;
var
  ABorder: TdxSpreadSheetCellStyleEditDialogBorder;
  ABorderInfo: TdxSpreadSheetCellStyleEditDialogBorderInfo;
begin
  for ABorder := Low(ABorder) to High(ABorder) do
  begin
    ABorderInfo := BordersPreview.BordersInfo[ABorder];
    if ABorderInfo.Assigned and not (ABorderInfo.Undefined or ABorderInfo.Empty) then
    begin
      ccbBorderLineColor.EditValue := ABorderInfo.Color;
      lbLineStyles.ItemIndex := Ord(ABorderInfo.Style);
      Break;
    end;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.SetupPages;
begin
  lcgBorders.Visible := csecBorder in Controller.Capabilities;
  lcgFill.Visible := csecFill in Controller.Capabilities;
  lcgFont.Visible := csecFont in Controller.Capabilities;
  lcgNumber.Visible := csecNumber in Controller.Capabilities;
  lcgProtection.Visible := csecProtection in Controller.Capabilities;
  lcgTextAlignment.Visible := csecAlignment in Controller.Capabilities;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.UpdateBordersButtons;
var
  AIndex: TdxSpreadSheetCellStyleEditDialogBorder;
  AInfo: TdxSpreadSheetCellStyleEditDialogBorderInfo;
begin
  for AIndex := Low(AIndex) to High(AIndex) do
  begin
    AInfo := BordersPreview.BordersInfo[AIndex];
    FBordersButtons[AIndex].Enabled := AInfo.Visible;
    FBordersButtons[AIndex].Down := not ((AInfo.Style in [sscbsDefault, sscbsNone]) or AInfo.Undefined);
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.UpdateFillSettings;
begin
  cgFill.ColorValue := pbFillPreview.Tag;
  btnFillNoColor.Down := pbFillPreview.Tag = clDefault;
  pbFillPreview.Invalidate;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.UpdateFontNameWarning;
begin
  if lbFontNames.ItemIndex < 0 then
    lbFontWarning.Caption := cxGetResourceString(@sdxFormatCellsDialogFontNotInstalled)
  else
    lbFontWarning.Caption := cxGetResourceString(@sdxFormatCellsDialogFontPrintNotes)
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.UpdateNumberFormat;
begin
  case NumberFormatCategory of
    nfcNumber:
      NumberFormat := TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.BuildCodeForNumber(seDecimalPlaces.Value, cbUseThousandSeparator.Checked);
    nfcPercentage:
      NumberFormat := TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.BuildCodeForPercentage(seDecimalPlaces.Value);
    nfcScientific:
      NumberFormat := TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.BuildCodeForScientific(seDecimalPlaces.Value);
    nfcCustom:
      NumberFormat := teCustomFormatCode.EditingText;
    nfcText:
      NumberFormat := '@';
    nfcCurrency, nfcAccounting, nfcDate, nfcTime, nfcFraction:
      NumberFormat := Helper.DisplayFormatCodeToFormatCode(lbNumberFormatTemplates);
  else
    NumberFormat := dxSpreadSheetGeneralNumberFormat;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.UpdateNumberFormatCategoryDescription;
begin
  case NumberFormatCategory of
    nfcNumber:
      lbCategoryDescription.Caption := cxGetResourceString(@sdxFormatCellsDialogCategoryNumberDescription);
    nfcCurrency:
      lbCategoryDescription.Caption := cxGetResourceString(@sdxFormatCellsDialogCategoryCurrencyDescription);
    nfcAccounting:
      lbCategoryDescription.Caption := cxGetResourceString(@sdxFormatCellsDialogCategoryAccountingDescription);
    nfcDate:
      lbCategoryDescription.Caption := cxGetResourceString(@sdxFormatCellsDialogCategoryDateDescription);
    nfcTime:
      lbCategoryDescription.Caption := cxGetResourceString(@sdxFormatCellsDialogCategoryTimeDescription);
    nfcPercentage:
      lbCategoryDescription.Caption := cxGetResourceString(@sdxFormatCellsDialogCategoryPercentageDescription);
    nfcCustom:
      lbCategoryDescription.Caption := cxGetResourceString(@sdxFormatCellsDialogCategoryCustomDescription);
  else
    lbCategoryDescription.Caption := '';
  end;

  case NumberFormatCategory of
    nfcGeneral:
      lbGeneralNotes.Caption := cxGetResourceString(@sdxFormatCellsDialogCategoryGeneralNotes);
    nfcText:
      lbGeneralNotes.Caption := cxGetResourceString(@sdxFormatCellsDialogCategoryTextNotes);
  end;

  case NumberFormatCategory of
    nfcDate, nfcCustom:
      lbCategoryDescription.Caption := lbCategoryDescription.Caption + ' ' +
        cxGetResourceString(@sdxFormatCellsDialogCategoryDateNote);
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.UpdateNumberFormatPreview;
var
  AResult: TdxSpreadSheetNumberFormatResult;
begin
  AResult := Controller.FormatFocusedCellValue(NumberFormat);
  lbSample.Caption := AResult.Text;
  lbSample.Style.TextColor := cxGetActualColor(AResult.Color, lbSample.Style.LookAndFeel.Painter.DefaultContentTextColor);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.UpdateShrinkToFitState;
begin
  lciShrinkToFit.Enabled := not (cbWrapText.Checked or
    (TdxSpreadSheetDataAlignHorz(ccbTextAlignHorz.ItemObject) in [ssahJustify, ssahFill, ssahDistributed]));
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.Load(
  ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect);
begin
  LoadBorders(ACellStyle, ARow, AColumn, AArea);
  LoadBrush(ACellStyle.Brush);
  LoadFont(ACellStyle.Font);
  LoadTextAlignment(ACellStyle, ARow, AColumn);
  LoadProtection(ACellStyle);
  LoadNumberFormat(ACellStyle.DataFormat);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.LoadBorders(
  ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect);
var
  ABorder: TdxSpreadSheetCellStyleEditDialogBorder;
begin
  for ABorder := Low(ABorder) to High(ABorder) do
    BordersPreview.BordersInfo[ABorder].Assigned := False;
  MergeBorders(ACellStyle, ARow, AColumn, AArea);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.LoadBrush(ABrush: TdxSpreadSheetCellBrush);
begin
  pbFillPreview.Tag := ABrush.BackgroundColor;
  ccbPatternColor.ColorValue := ABrush.ForegroundColor;
  icbPatternStyle.ItemIndex := Ord(ABrush.Style);
  UpdateFillSettings;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.LoadFont(AFont: TdxSpreadSheetCellFont);
begin
  LoadFont(TdxSpreadSheetCellFontAccess(AFont).Handle);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.LoadFont(AFont: TdxSpreadSheetFontHandle);
begin
  cbFontColor.EditValue := AFont.Color;
  teFontName.EditValue := AFont.Name;
  teFontSize.EditValue := AFont.Size;
  cbFontUnderline.ItemIndex := Ord(fsUnderline in AFont.Style);
  cbFontStrikethrough.Checked := fsStrikeOut in AFont.Style;
  teFontStyle.EditValue := Helper.GetFontStyleName(AFont.Style);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.LoadNumberFormat(ADataFormat: TdxSpreadSheetCellDataFormat);
var
  ADecimalPlaces: Integer;
  AUseThousandSeparator: Boolean;
begin
  if ADataFormat.FormatCode = '@' then
    NumberFormatCategory := nfcText
  else

  if TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.IsGeneralCode(ADataFormat.FormatCode) then
    NumberFormatCategory := nfcGeneral
  else

  if TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.IsNumberCode(ADataFormat.FormatCode, ADecimalPlaces, AUseThousandSeparator) then
  begin
    NumberFormatCategory := nfcNumber;
    cbUseThousandSeparator.Checked := AUseThousandSeparator;
    seDecimalPlaces.Value := ADecimalPlaces;
  end
  else

  if TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.IsPercentageCode(ADataFormat.FormatCode, ADecimalPlaces) then
  begin
    NumberFormatCategory := nfcPercentage;
    seDecimalPlaces.Value := ADecimalPlaces;
  end
  else

  if TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.IsScientificCode(ADataFormat.FormatCode, ADecimalPlaces) then
  begin
    NumberFormatCategory := nfcScientific;
    seDecimalPlaces.Value := ADecimalPlaces;
  end
  else
  begin
    case Controller.PredefinedFormats.GetIDByFormatCode(ADataFormat.FormatCode) of
      $05, $06, $07, $08:
        NumberFormatCategory := nfcCurrency;
      $29, $2A, $2B, $2C:
        NumberFormatCategory := nfcAccounting;
      $0E, $0F, $10, $11, $16:
        NumberFormatCategory := nfcDate;
      $12, $13, $14, $15, $2D, $2E, $2F:
        NumberFormatCategory := nfcTime;
      $0C, $0D:
        NumberFormatCategory := nfcFraction;
    else
      NumberFormatCategory := nfcCustom;
    end;
    lbNumberFormatTemplates.ItemIndex := lbNumberFormatTemplates.Items.IndexOf(Helper.FormatCodeToDisplayFormatCode(ADataFormat));
    teCustomFormatCode.Text := ADataFormat.FormatCode;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.LoadProtection(ACellStyle: TdxSpreadSheetCellStyle);
begin
  cbHidden.Checked := ACellStyle.Hidden;
  cbLocked.Checked := ACellStyle.Locked;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.LoadTextAlignment(ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer);
begin
  seTextAlignHorzIndent.EditValue := ACellStyle.AlignHorzIndent;
  ccbTextAlignHorz.ItemObject := TObject(ACellStyle.AlignHorz);
  ccbTextAlignVert.ItemObject := TObject(ACellStyle.AlignVert);
  cbShrinkToFit.Checked := ACellStyle.ShrinkToFit;
  cbWrapText.Checked := ACellStyle.WordWrap;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.Merge(
  ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect);
begin
  MergeBorders(ACellStyle, ARow, AColumn, AArea);
  MergeBrush(ACellStyle.Brush);
  MergeFont(ACellStyle.Font);
  MergeNumberFormat(ACellStyle.DataFormat);
  MergeTextAlignment(ACellStyle, ARow, AColumn);
  MergeProtection(ACellStyle);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.MergeBorders(
  ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect);
var
  ABorder: TcxBorder;
  ABorderInfo: TdxSpreadSheetCellStyleEditDialogBorderInfo;
begin
  for ABorder := Low(TcxBorder) to High(TcxBorder) do
  begin
    ABorderInfo := BordersPreview.BordersInfo[Helper.GetActualBorderType(ABorder, ARow, AColumn, AArea)];
    if ABorderInfo.Assigned then
    begin
      ABorderInfo.Undefined := ABorderInfo.Undefined or
        (ABorderInfo.Color <> ACellStyle.Borders[ABorder].Color) or
        (ABorderInfo.Style <> ACellStyle.Borders[ABorder].Style);
    end
    else
    begin
      ABorderInfo.Color := ACellStyle.Borders[ABorder].Color;
      ABorderInfo.Style := ACellStyle.Borders[ABorder].Style;
      ABorderInfo.Assigned := True;
    end;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.MergeBrush(ABrush: TdxSpreadSheetCellBrush);
begin
  MergeEditValue(ccbPatternColor, ABrush.ForegroundColor);
  if pbFillPreview.Tag <> ABrush.BackgroundColor then
    pbFillPreview.Tag := clNone;
  if icbPatternStyle.ItemIndex <> Ord(ABrush.Style) then
    icbPatternStyle.ItemIndex := -1;
  UpdateFillSettings;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.MergeEditValue(AEdit: TcxCustomEdit; const AValue: Variant);
begin
  if (AEdit.EditValue <> AValue) and (AEdit.EditValue <> Null) then
    AEdit.EditValue := Null;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.MergeFont(AFont: TdxSpreadSheetCellFont);
begin
  if cbFontUnderline.ItemIndex <> Ord(fsUnderline in AFont.Style) then
    cbFontUnderline.ItemIndex := -1;
  MergeEditValue(cbFontColor, AFont.Color);
  MergeEditValue(teFontName, AFont.Name);
  MergeEditValue(teFontSize, AFont.Size);
  MergeEditValue(teFontStyle, Helper.GetFontStyleName(AFont.Style));
  if (cbFontStrikethrough.State <> cbsGrayed) and (cbFontStrikethrough.Checked <> (fsStrikeOut in AFont.Style)) then
    cbFontStrikethrough.State := cbsGrayed;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.MergeNumberFormat(ADataFormat: TdxSpreadSheetCellDataFormat);
begin
  if NumberFormat <> ADataFormat.FormatCode then
  begin
    if lbCategory.ItemIndex <> -1 then
    begin
      lbCategory.ItemIndex := -1;
      lbCategoryClick(nil);
    end;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.MergeProtection(ACellStyle: TdxSpreadSheetCellStyle);
begin
  if cbHidden.Checked <> ACellStyle.Hidden then
    cbHidden.State := cbsGrayed;
  if cbLocked.Checked <> ACellStyle.Locked then
    cbLocked.State := cbsGrayed;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.MergeTextAlignment(ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer);
begin
  MergeEditValue(seTextAlignHorzIndent, ACellStyle.AlignHorzIndent);
  if ccbTextAlignHorz.ItemObject <> TObject(ACellStyle.AlignHorz) then
    ccbTextAlignHorz.ItemIndex := -1;
  if ccbTextAlignVert.ItemObject <> TObject(ACellStyle.AlignVert) then
    ccbTextAlignVert.ItemIndex := -1;
  if cbShrinkToFit.Checked <> ACellStyle.ShrinkToFit then
    cbShrinkToFit.State := cbsGrayed;
  if cbWrapText.Checked <> ACellStyle.WordWrap then
    cbWrapText.State := cbsGrayed;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.Save(ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect);
begin
  ACellStyle.BeginUpdate;
  try
    SaveBorders(ACellStyle, ARow, AColumn, AArea);
    SaveBrush(ACellStyle.Brush);
    SaveTextAlignment(ACellStyle);
    SaveNumberFormat(ACellStyle.DataFormat);
    SaveFont(ACellStyle.Font);
    SaveProtection(ACellStyle);
  finally
    ACellStyle.EndUpdate;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.SaveBorders(
  ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect);
var
  ABorder: TcxBorder;
  ABorderInfo: TdxSpreadSheetCellStyleEditDialogBorderInfo;
begin
  for ABorder := Low(TcxBorder) to High(TcxBorder) do
  begin
    ABorderInfo := BordersPreview.BordersInfo[Helper.GetActualBorderType(ABorder, ARow, AColumn, AArea)];
    if ABorderInfo.Assigned and not ABorderInfo.Undefined then
    begin
      ACellStyle.Borders[ABorder].Style := ABorderInfo.Style;
      if ABorderInfo.Style in [sscbsDefault, sscbsNone] then
        ACellStyle.Borders[ABorder].Color := clDefault
      else
        ACellStyle.Borders[ABorder].Color := ABorderInfo.Color;
    end;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.SaveBrush(ABrush: TdxSpreadSheetCellBrush);
begin
  if pbFillPreview.Tag <> clNone then
    ABrush.BackgroundColor := pbFillPreview.Tag;
  if not VarIsNull(ccbPatternColor.EditValue) then
    ABrush.ForegroundColor := Helper.ColorComboCache.GetValue(ccbPatternColor);
  if icbPatternStyle.ItemIndex >= 0 then
    ABrush.Style := TdxSpreadSheetCellFillStyle(icbPatternStyle.ItemIndex);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.SaveFont(AFont: TdxSpreadSheetCellFont);
var
  AValue: Integer;
begin
  if cbFontUnderline.ItemIndex >= 0 then
  begin
    if cbFontUnderline.ItemIndex <> 0 then
      AFont.Style := AFont.Style + [fsUnderline]
    else
      AFont.Style := AFont.Style - [fsUnderline];
  end;

  if cbFontStrikethrough.State <> cbsGrayed then
  begin
    if cbFontStrikethrough.Checked then
      AFont.Style := AFont.Style + [fsStrikeOut]
    else
      AFont.Style := AFont.Style - [fsStrikeOut];
  end;

  if not VarIsNull(teFontStyle.EditValue) then
    AFont.Style := AFont.Style - [fsBold, fsItalic] + Helper.GetFontStyle(Helper.ListBoxCache.GetValue(lbFontStyles));

  if not VarIsNull(cbFontColor.EditValue) then
    AFont.Color := Helper.ColorComboCache.GetValue(cbFontColor);

  if not VarIsNull(teFontName.EditValue) then
  begin
    AFont.Name := teFontName.EditValue;
    AFont.Charset := DEFAULT_CHARSET;
  end;

  if not VarIsNull(teFontSize.EditingValue) then
    if TryStrToInt(teFontSize.EditValue, AValue) then
      AFont.Size := AValue;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.SaveNumberFormat(ADataFormat: TdxSpreadSheetCellDataFormat);
begin
  if Helper.ListBoxCache.GetValue(lbCategory) >= 0 then
  begin
    if NumberFormat <> '' then
      ADataFormat.FormatCode := NumberFormat
    else
      ADataFormat.FormatCode := dxSpreadSheetGeneralNumberFormat;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.SaveProtection(ACellStyle: TdxSpreadSheetCellStyle);
begin
  if cbHidden.State <> cbsGrayed then
    ACellStyle.Hidden := cbHidden.Checked;
  if cbLocked.State <> cbsGrayed then
    ACellStyle.Locked := cbLocked.Checked;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.SaveTextAlignment(ACellStyle: TdxSpreadSheetCellStyle);
begin
  if not VarIsNull(seTextAlignHorzIndent.EditValue) then
    ACellStyle.AlignHorzIndent := seTextAlignHorzIndent.EditValue;
  if ccbTextAlignHorz.ItemIndex >= 0 then
    ACellStyle.AlignHorz := TdxSpreadSheetDataAlignHorz(ccbTextAlignHorz.ItemObject);
  if ccbTextAlignVert.ItemIndex >= 0 then
    ACellStyle.AlignVert := TdxSpreadSheetDataAlignVert(ccbTextAlignVert.ItemObject);
  if cbShrinkToFit.State <> cbsGrayed then
    ACellStyle.ShrinkToFit := cbShrinkToFit.Checked;
  if cbWrapText.State <> cbsGrayed then
    ACellStyle.WordWrap := cbWrapText.Checked;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.NumberFormatChanged(Sender: TObject);
begin
  UpdateNumberFormat;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.BordersPreviewChanged(Sender: TObject);
begin
  UpdateBordersButtons;
  pbBordersPreview.Invalidate;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.FontLoaderCompleted(Sender: TObject);
begin
  lbFontNames.Items.Assign(FFontLoader.FontList);
  lbFontNames.ItemIndex := lbFontNames.Items.IndexOf(teFontName.Text);
  UpdateFontNameWarning;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.FontLoaderDestroyed(Sender: TObject);
begin
  FFontLoader := nil;
end;

function TdxSpreadSheetCellStyleEditDialogForm.GetNumberFormatCategory: TdxSpreadSheetNumberFormatCategory;
begin
  Result := TdxSpreadSheetNumberFormatCategory(lbCategory.ItemObject);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.SetNumberFormat(const AValue: string);
begin
  if FNumberFormat <> AValue then
  begin
    FNumberFormat := AValue;
    UpdateNumberFormatPreview;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.SetNumberFormatCategory(const AValue: TdxSpreadSheetNumberFormatCategory);
begin
  lbCategory.ItemObject := TObject(AValue);
  lbCategoryClick(nil);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.teFontNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Helper.NavigateOnListbox(Key, lbFontNames);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.teFontNamePropertiesChange(Sender: TObject);
begin
  if not lbFontNames.Focused then
  begin
    lbFontNames.ItemIndex := lbFontNames.Items.IndexOf(teFontName.Text);
    UpdateFontNameWarning;
  end;
  cbFontChanged(Sender);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.teFontSizeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Helper.NavigateOnListbox(Key, lbFontSizes);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.teFontSizePropertiesChange(Sender: TObject);
begin
  if not lbFontSizes.Focused then
    lbFontSizes.ItemIndex := lbFontSizes.Items.IndexOf(teFontSize.Text);
  cbFontChanged(Sender);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.teFontStyleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Helper.NavigateOnListbox(Key, lbFontStyles);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.teFontStylePropertiesChange(Sender: TObject);
begin
  if not lbFontStyles.Focused then
    lbFontStyles.ItemIndex := lbFontStyles.Items.IndexOf(teFontStyle.Text);
  cbFontChanged(Sender);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.btnBorderClick(Sender: TObject);
var
  ABorderInfo: TdxSpreadSheetCellStyleEditDialogBorderInfo;
  AColor: TColor;
  AStyle: TdxSpreadSheetCellBorderStyle;
begin
  if lbLineStyles.ItemIndex >= 0 then
  begin
    AStyle := TdxSpreadSheetCellBorderStyle(lbLineStyles.ItemObject);
    AColor := Helper.GetActualColor(ccbBorderLineColor);
    ABorderInfo := BordersPreview.BordersInfo[TdxSpreadSheetCellStyleEditDialogBorder(TComponent(Sender).Tag)];
    ABorderInfo.Assigned := True;
    if (ABorderInfo.Style <> AStyle) or (ABorderInfo.Color <> AColor) or ABorderInfo.Undefined then
    begin
      ABorderInfo.Color := AColor;
      ABorderInfo.Style := AStyle;
      ABorderInfo.Undefined := False;
    end
    else
      ABorderInfo.Style := sscbsDefault;
  end;
  UpdateBordersButtons;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.btnBordersInsideClick(Sender: TObject);
begin
  if lbLineStyles.ItemIndex >= 0 then
    SetBordersStyle([fcdbInsideHorizontal, fcdbInsideVertical], TdxSpreadSheetCellBorderStyle(lbLineStyles.ItemObject));
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.btnBordersNoneClick(Sender: TObject);
begin
  SetBordersStyle([Low(TdxSpreadSheetCellStyleEditDialogBorder)..High(TdxSpreadSheetCellStyleEditDialogBorder)], sscbsDefault);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.btnBordersOutlineClick(Sender: TObject);
begin
  if lbLineStyles.ItemIndex >= 0 then
    SetBordersStyle([fcdbLeft..fcdbBottom], TdxSpreadSheetCellBorderStyle(lbLineStyles.ItemObject));
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.btnFillMoreColorsClick(Sender: TObject);
begin
  ColorDialog.Color := dxColorToAlphaColor(pbFillPreview.Tag);
  if ColorDialog.Execute(Handle) then
  begin
    pbFillPreview.Tag := dxAlphaColorToColor(ColorDialog.Color);
    UpdateFillSettings;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.btnFillNoColorClick(Sender: TObject);
begin
  pbFillPreview.Tag := clDefault;
  UpdateFillSettings;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.btnFontResetClick(Sender: TObject);
begin
  LoadFont(Controller.DefaultFont);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.cbColorComboBoxButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  if AButtonIndex = 2 then
    (Sender as TcxColorComboBox).ColorValue := clDefault;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.cbColorComboBoxNamingConvention(
  Sender: TObject; const AColor: TColor; var AColorDescription: string);
begin
  if AColor = clDefault then
    AColorDescription := cxGetResourceString(@sdxFormatCellsDialogAuto)
  else
    if AColorDescription = '' then
      AColorDescription := '$' + IntToHex(AColor, 6);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.cbFillChanged(Sender: TObject);
begin
  UpdateFillSettings;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.cbFontChanged(Sender: TObject);

  procedure SaveFont(AFont: TFont);
  var
    AValue: Integer;
  begin
    AFont.PixelsPerInch := dxDefaultDPI;

    Controller.DefaultFont.AssignToFont(AFont);
    if cbFontUnderline.ItemIndex >= 0 then
    begin
      if cbFontUnderline.ItemIndex <> 0 then
        AFont.Style := AFont.Style + [fsUnderline]
      else
        AFont.Style := AFont.Style - [fsUnderline];
    end;

    if cbFontStrikethrough.State <> cbsGrayed then
    begin
      if cbFontStrikethrough.Checked then
        AFont.Style := AFont.Style + [fsStrikeOut]
      else
        AFont.Style := AFont.Style - [fsStrikeOut];
    end;

    if not VarIsNull(teFontStyle.EditingValue) then
      AFont.Style := AFont.Style - [fsBold, fsItalic] + Helper.GetFontStyle(lbFontStyles.ItemIndex);

    if not VarIsNull(cbFontColor.EditingValue) then
      AFont.Color := cbFontColor.ColorValue;

    if not VarIsNull(teFontName.EditingValue) then
      AFont.Name := teFontName.EditingValue;

    if not VarIsNull(teFontSize.EditingValue) then
    begin
      if TryStrToInt(teFontSize.EditingValue, AValue) then
        AFont.Size := ScaleFactor.Apply(AValue);
    end;
  end;

begin
  SaveFont(pbTextPreview.Font);
  pbTextPreview.Invalidate;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.cbWrapTextPropertiesChange(Sender: TObject);
begin
  UpdateShrinkToFitState;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.ccbBorderLineColorPropertiesChange(Sender: TObject);
begin
  lbLineStyles.Invalidate;
  pbBordersPreview.Invalidate;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.ccbTextAlignHorzPropertiesChange(Sender: TObject);
begin
  lciTextAlignHorzIndent.Enabled := TdxSpreadSheetDataAlignHorz(ccbTextAlignHorz.ItemObject) in [ssahLeft, ssahRight, ssahDistributed];
  UpdateShrinkToFitState;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.cgFillItemClick(Sender: TObject; AItem: TdxGalleryControlItem);
begin
  pbFillPreview.Tag := cgFill.ColorValue;
  UpdateFillSettings;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.clbFontStylesClickCheck(
  Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
begin
  UpdateFillSettings;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.lbCategoryClick(Sender: TObject);
begin
  lcMain.BeginUpdate;
  try
    lciSample.Visible := lbCategory.ItemIndex >= 0;
    lciCustomFormatCode.Visible := NumberFormatCategory = nfcCustom;
    lciDecimalPlaces.Visible := NumberFormatCategory in [nfcNumber, nfcPercentage, nfcScientific];
    lciGeneralNotes.Visible := (lbCategory.ItemIndex >= 0) and (NumberFormatCategory in [nfcGeneral, nfcText]);
    lciNumberFormatTemplates.CaptionOptions.Visible := NumberFormatCategory <> nfcCustom;
    lciNumberFormatTemplates.Visible := NumberFormatCategory in [nfcCurrency, nfcAccounting, nfcDate, nfcTime, nfcCustom, nfcFraction];
    lciUseThousandSeparator.Visible := NumberFormatCategory in [nfcNumber];
    Helper.PopulateNumberFormatTemplates(lbNumberFormatTemplates, NumberFormatCategory);
    UpdateNumberFormat;
    UpdateNumberFormatCategoryDescription;
  finally
    lcMain.EndUpdate(False);
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.lbFontNamesClick(Sender: TObject);
begin
  Helper.SyncSearchEdit(lbFontNames, teFontName);
  UpdateFontNameWarning;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.lbFontSizesClick(Sender: TObject);
begin
  Helper.SyncSearchEdit(lbFontSizes, teFontSize);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.lbFontStylesClick(Sender: TObject);
begin
  Helper.SyncSearchEdit(lbFontStyles, teFontStyle);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.lbLineStylesDrawItem(
  AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
begin
  Helper.DrawBorderStylePattern(AControl, ACanvas,
    Helper.GetActualColor(ccbBorderLineColor), ARect,
    TdxSpreadSheetCellBorderStyle(AIndex));
  if (AIndex = AControl.ItemIndex) and not AControl.Focused then
    ACanvas.DrawFocusRect(ARect);
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.lbNumberFormatTemplatesClick(Sender: TObject);
begin
  if NumberFormatCategory = nfcCustom then
    teCustomFormatCode.Text := Helper.DisplayFormatCodeToFormatCode(lbNumberFormatTemplates)
  else
    UpdateNumberFormat;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.pbBordersPreviewMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ABorder: TdxSpreadSheetCellStyleEditDialogBorder;
begin
  BordersPreview.Bounds := pbBordersPreview.ClientRect;
  if BordersPreview.HitTest(Point(X, Y), ABorder) then
    FBordersButtons[ABorder].Click;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.pbBordersPreviewPaint(Sender: TObject);
begin
  cxPaintCanvas.BeginPaint(pbBordersPreview.Canvas);
  try
    FBordersPreview.Bounds := pbBordersPreview.ClientRect;
    FBordersPreview.Draw(cxPaintCanvas);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.pbFillPreviewPaint(Sender: TObject);
begin
  if pbFillPreview.Tag <> clNone then
  begin
    cxPaintCanvas.BeginPaint(pbFillPreview.Canvas);
    try
      dxSpreadSheetDrawBackground(cxPaintCanvas, pbFillPreview.ClientRect,
        cxGetActualColor(pbFillPreview.Tag, clWindow), ccbPatternColor.ColorValue,
        TdxSpreadSheetCellFillStyle(icbPatternStyle.ItemIndex));
    finally
      cxPaintCanvas.EndPaint;
    end;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogForm.pbTextPreviewPaint(Sender: TObject);
var
  R: TRect;
begin
  R := pbTextPreview.ClientRect;
  pbTextPreview.Canvas.Brush.Color := clWindow;
  pbTextPreview.Canvas.FillRect(pbTextPreview.ClientRect);
  cxTextOut(pbTextPreview.Canvas, cxGetResourceString(@sdxPreviewText), R, CXTO_CENTER_HORIZONTALLY or CXTO_CENTER_VERTICALLY);
end;

initialization

finalization
  FreeAndNil(FCustomColors);
end.
