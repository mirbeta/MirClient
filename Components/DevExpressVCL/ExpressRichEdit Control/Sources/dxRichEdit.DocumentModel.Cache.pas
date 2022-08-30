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

unit dxRichEdit.DocumentModel.Cache;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.ProtectionFormatting,
  dxRichEdit.DocumentModel.DocumentProperties,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.Cache.Simple;

type
  { TdxDocumentCache }

  TdxDocumentCache = class(TdxSimpleDocumentCache)
  strict private
    FParagraphFrameFormattingInfoCache: TdxParagraphFrameFormattingInfoCache;
    FMergedParagraphFrameFormattingInfoCache: TdxParagraphFrameFormattingInfoCache;
    FGeneralSectionInfoCache: TdxGeneralSectionInfoCache;
    FPageInfoCache: TdxPageInfoCache;
    FListLevelInfoCache: TdxListLevelInfoCache;
    FMarginsInfoCache: TdxMarginsInfoCache;
    FColumnsInfoCache: TdxColumnsInfoCache;
    FPageNumberingInfoCache: TdxPageNumberingInfoCache;
    FLineNumberingInfoCache: TdxLineNumberingInfoCache;

    FTablePropertiesOptionsCache: TdxTablePropertiesOptionsCache;

    FTableGeneralSettingsInfoCache: TdxTableGeneralSettingsInfoCache;
    FTableFloatingPositionInfoCache: TdxTableFloatingPositionInfoCache;
    FTableCellGeneralSettingsInfoCache: TdxTableCellGeneralSettingsInfoCache;
    FTableCellPropertiesOptionsCache: TdxTableCellPropertiesOptionsCache;

    FTableRowGeneralSettingsInfoCache: TdxTableRowGeneralSettingsInfoCache;
    FTableRowPropertiesOptionsCache: TdxTableRowPropertiesOptionsCache;
    FRangePermissionInfoCache: TdxRangePermissionInfoCache;
    FDocumentProtectionInfoCache: TdxDocumentProtectionInfoCache;

    FFootNoteInfoCache: TdxFootNoteInfoCache;

    FFloatingObjectInfoCache: TdxFloatingObjectInfoCache;
    FFloatingObjectFormattingCache: TdxFloatingObjectFormattingCache;

    FShapeInfoCache: TdxShapeInfoCache;
    FShapeFormattingCache: TdxShapeFormattingCache;

    FTextBoxInfoCache: TdxTextBoxInfoCache;
    FTextBoxFormattingCache: TdxTextBoxFormattingCache;
  public
    destructor Destroy; override;
    procedure Initialize(ADocumentModel: TdxCustomDocumentModel); override;
    property MarginsInfoCache: TdxMarginsInfoCache read FMarginsInfoCache;
    property ColumnsInfoCache: TdxColumnsInfoCache read FColumnsInfoCache;
    property PageInfoCache: TdxPageInfoCache read FPageInfoCache;
    property GeneralSectionInfoCache: TdxGeneralSectionInfoCache read FGeneralSectionInfoCache;
    property PageNumberingInfoCache: TdxPageNumberingInfoCache read FPageNumberingInfoCache;
    property LineNumberingInfoCache: TdxLineNumberingInfoCache read FLineNumberingInfoCache;
    property ParagraphFrameFormattingInfoCache: TdxParagraphFrameFormattingInfoCache read FParagraphFrameFormattingInfoCache;
    property MergedParagraphFrameFormattingInfoCache: TdxParagraphFrameFormattingInfoCache read FMergedParagraphFrameFormattingInfoCache;
    property TableFloatingPositionInfoCache: TdxTableFloatingPositionInfoCache read FTableFloatingPositionInfoCache;
    property TableCellGeneralSettingsInfoCache: TdxTableCellGeneralSettingsInfoCache read FTableCellGeneralSettingsInfoCache;
    property TableGeneralSettingsInfoCache: TdxTableGeneralSettingsInfoCache read FTableGeneralSettingsInfoCache;
    property TablePropertiesOptionsCache: TdxTablePropertiesOptionsCache read FTablePropertiesOptionsCache;
    property TableRowPropertiesOptionsCache: TdxTableRowPropertiesOptionsCache read FTableRowPropertiesOptionsCache;
    property TableRowGeneralSettingsInfoCache: TdxTableRowGeneralSettingsInfoCache read FTableRowGeneralSettingsInfoCache;
    property TableCellPropertiesOptionsCache: TdxTableCellPropertiesOptionsCache read FTableCellPropertiesOptionsCache;
    property RangePermissionInfoCache: TdxRangePermissionInfoCache read FRangePermissionInfoCache;
    property DocumentProtectionInfoCache: TdxDocumentProtectionInfoCache read FDocumentProtectionInfoCache;
    property FootNoteInfoCache: TdxFootNoteInfoCache read FFootNoteInfoCache;
    property FloatingObjectInfoCache: TdxFloatingObjectInfoCache read FFloatingObjectInfoCache;
    property ListLevelInfoCache: TdxListLevelInfoCache read FListLevelInfoCache;
    property FloatingObjectFormattingCache: TdxFloatingObjectFormattingCache read FFloatingObjectFormattingCache;
    property ShapeInfoCache: TdxShapeInfoCache read FShapeInfoCache;
    property ShapeFormattingCache: TdxShapeFormattingCache read FShapeFormattingCache;
    property TextBoxInfoCache: TdxTextBoxInfoCache read FTextBoxInfoCache;
    property TextBoxFormattingCache: TdxTextBoxFormattingCache read FTextBoxFormattingCache;
  end;

implementation

{ TdxDocumentCache }

destructor TdxDocumentCache.Destroy;
begin
  FreeAndNil(FRangePermissionInfoCache);
  FreeAndNil(FDocumentProtectionInfoCache);
  FreeAndNil(FMarginsInfoCache);
  FreeAndNil(FColumnsInfoCache);
  FreeAndNil(FPageInfoCache);
  FreeAndNil(FLineNumberingInfoCache);
  FreeAndNil(FPageNumberingInfoCache);
  FreeAndNil(FGeneralSectionInfoCache);
  FreeAndNil(FParagraphFrameFormattingInfoCache);
  FreeAndNil(FMergedParagraphFrameFormattingInfoCache);
  FreeAndNil(FTablePropertiesOptionsCache);
  FreeAndNil(FTableFloatingPositionInfoCache);
  FreeAndNil(FTableCellGeneralSettingsInfoCache);
  FreeAndNil(FTableCellPropertiesOptionsCache);
  FreeAndNil(FTableGeneralSettingsInfoCache);
  FreeAndNil(FListLevelInfoCache);
  FreeAndNil(FTableRowPropertiesOptionsCache);
  FreeAndNil(FTableRowGeneralSettingsInfoCache);
  FreeAndNil(FFootNoteInfoCache);
  FreeAndNil(FFloatingObjectInfoCache);
  FreeAndNil(FFloatingObjectFormattingCache);
  FreeAndNil(FShapeInfoCache);
  FreeAndNil(FShapeFormattingCache);
  FreeAndNil(FTextBoxInfoCache);
  FreeAndNil(FTextBoxFormattingCache);
  inherited Destroy;
end;

procedure TdxDocumentCache.Initialize(ADocumentModel: TdxCustomDocumentModel);
var
  AUnitConverter: TdxDocumentModelUnitConverter;
begin
  inherited Initialize(ADocumentModel);
  Assert(ADocumentModel <> nil);
  AUnitConverter := ADocumentModel.UnitConverter;

  FMarginsInfoCache := TdxMarginsInfoCache.Create(AUnitConverter);
  FColumnsInfoCache := TdxColumnsInfoCache.Create(AUnitConverter);

  FPageInfoCache := TdxPageInfoCache.Create(AUnitConverter);
  FGeneralSectionInfoCache := TdxGeneralSectionInfoCache.Create(AUnitConverter);
  FPageNumberingInfoCache := TdxPageNumberingInfoCache.Create(AUnitConverter);
  FLineNumberingInfoCache := TdxLineNumberingInfoCache.Create(AUnitConverter);

  FParagraphFrameFormattingInfoCache := TdxParagraphFrameFormattingInfoCache.Create(AUnitConverter);
  FMergedParagraphFrameFormattingInfoCache := TdxParagraphFrameFormattingInfoCache.Create(AUnitConverter);

  FTablePropertiesOptionsCache := TdxTablePropertiesOptionsCache.Create(AUnitConverter);
  FListLevelInfoCache := TdxListLevelInfoCache.Create(AUnitConverter);
  FTableGeneralSettingsInfoCache := TdxTableGeneralSettingsInfoCache.Create(AUnitConverter);

  FTableFloatingPositionInfoCache := TdxTableFloatingPositionInfoCache.Create(AUnitConverter);
  FTableCellGeneralSettingsInfoCache := TdxTableCellGeneralSettingsInfoCache.Create(AUnitConverter);
  FTableCellPropertiesOptionsCache := TdxTableCellPropertiesOptionsCache.Create(AUnitConverter);

  FTableRowGeneralSettingsInfoCache := TdxTableRowGeneralSettingsInfoCache.Create(AUnitConverter);
  FTableRowPropertiesOptionsCache := TdxTableRowPropertiesOptionsCache.Create(AUnitConverter);

  FRangePermissionInfoCache := TdxRangePermissionInfoCache.Create(AUnitConverter);
  FDocumentProtectionInfoCache := TdxDocumentProtectionInfoCache.Create(AUnitConverter);
  FFootNoteInfoCache := TdxFootNoteInfoCache.Create(AUnitConverter);
  FFloatingObjectInfoCache := TdxFloatingObjectInfoCache.Create(AUnitConverter);
  FFloatingObjectFormattingCache := TdxFloatingObjectFormattingCache.Create(ADocumentModel);

  FShapeInfoCache := TdxShapeInfoCache.Create(AUnitConverter);
  FShapeFormattingCache := TdxShapeFormattingCache.Create(ADocumentModel);

  FTextBoxInfoCache := TdxTextBoxInfoCache.Create(AUnitConverter);
  FTextBoxFormattingCache := TdxTextBoxFormattingCache.Create(ADocumentModel);
end;

end.
