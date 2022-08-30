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

unit dxRichEdit.DocumentModel.Cache.Simple;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.InlineObjectFormatting,
  dxRichEdit.DocumentModel.DocumentProperties,
  dxRichEdit.DocumentModel.Borders;

type
  { TdxSimpleDocumentCache }

  TdxSimpleDocumentCache = class(TdxCustomDocumentCache)
  strict private
    FBorderInfoCache: TdxBorderInfoCache;
    FCharacterFormattingCache: TdxCharacterFormattingCache;
    FCharacterFormattingInfoCache: TdxCharacterFormattingInfoCache;
    FDocumentInfoCache: TdxDocumentInfoCache;
    FHeightUnitInfoCache: TdxHeightUnitInfoCache;
    FParagraphFrameFormattingCache: TdxParagraphFrameFormattingCache;
    FInlineCustomObjectInfoCache: TdxInlineCustomObjectInfoCache;
    FInlinePictureInfoCache: TdxInlinePictureInfoCache;
    FMergedCharacterFormattingInfoCache: TdxCharacterFormattingInfoCache;
    FMergedParagraphFormattingInfoCache: TdxParagraphFormattingInfoCache;
    FParagraphFormattingCache: TdxParagraphFormattingCache;
    FParagraphFormattingInfoCache: TdxParagraphFormattingInfoCache;
    FTabFormattingInfoCache: TdxTabFormattingInfoCache;
    FUnitInfoCache: TdxWidthUnitInfoCache;
  protected
    function CreateParagraphFormattingInfoCache(AUnitConverter:
      TdxDocumentModelUnitConverter): TdxParagraphFormattingInfoCache; virtual;
    function CreateParagraphFormattingCache(ADocumentModel: TdxCustomDocumentModel): TdxParagraphFormattingCache; virtual;
  public
    destructor Destroy; override;
    procedure Initialize(ADocumentModel: TdxCustomDocumentModel); override;

    property BorderInfoCache: TdxBorderInfoCache read FBorderInfoCache;
    property CharacterFormattingCache: TdxCharacterFormattingCache read FCharacterFormattingCache;
    property CharacterFormattingInfoCache: TdxCharacterFormattingInfoCache read FCharacterFormattingInfoCache;
    property DocumentInfoCache: TdxDocumentInfoCache read FDocumentInfoCache;
    property HeightUnitInfoCache: TdxHeightUnitInfoCache read FHeightUnitInfoCache;
    property InlineCustomObjectInfoCache: TdxInlineCustomObjectInfoCache read FInlineCustomObjectInfoCache;
    property InlinePictureInfoCache: TdxInlinePictureInfoCache read FInlinePictureInfoCache;
    property MergedCharacterFormattingInfoCache: TdxCharacterFormattingInfoCache read FMergedCharacterFormattingInfoCache;
    property MergedParagraphFormattingInfoCache: TdxParagraphFormattingInfoCache read FMergedParagraphFormattingInfoCache;
    property ParagraphFormattingCache: TdxParagraphFormattingCache read FParagraphFormattingCache;
    property ParagraphFormattingInfoCache: TdxParagraphFormattingInfoCache read FParagraphFormattingInfoCache;
    property ParagraphFrameFormattingCache: TdxParagraphFrameFormattingCache read FParagraphFrameFormattingCache;
    property TabFormattingInfoCache: TdxTabFormattingInfoCache read FTabFormattingInfoCache;
    property UnitInfoCache: TdxWidthUnitInfoCache read FUnitInfoCache;
  end;

implementation

{ TdxSimpleDocumentCache }

destructor TdxSimpleDocumentCache.Destroy;
begin
  FreeAndNil(FDocumentInfoCache);
  FreeAndNil(FCharacterFormattingCache);
  FreeAndNil(FCharacterFormattingInfoCache);
  FreeAndNil(FParagraphFormattingInfoCache);
  FreeAndNil(FParagraphFormattingCache);
  FreeAndNil(FInlinePictureInfoCache);
  FreeAndNil(FInlineCustomObjectInfoCache);
  FreeAndNil(FUnitInfoCache);
  FreeAndNil(FBorderInfoCache);
  FreeAndNil(FHeightUnitInfoCache);
  FreeAndNil(FMergedParagraphFormattingInfoCache);
  FreeAndNil(FMergedCharacterFormattingInfoCache);
  FreeAndNil(FTabFormattingInfoCache);
  FreeAndNil(FParagraphFrameFormattingCache);
  inherited Destroy;
end;

procedure TdxSimpleDocumentCache.Initialize(ADocumentModel: TdxCustomDocumentModel);
var
  AUnitConverter: TdxDocumentModelUnitConverter;
begin
  Assert(ADocumentModel <> nil);
  AUnitConverter := ADocumentModel.UnitConverter;
  FDocumentInfoCache := TdxDocumentInfoCache.Create(AUnitConverter);
  FMergedCharacterFormattingInfoCache := TdxCharacterFormattingInfoCache.Create(AUnitConverter);
  FCharacterFormattingInfoCache := TdxCharacterFormattingInfoCache.Create(AUnitConverter);
  FCharacterFormattingCache := TdxCharacterFormattingCache.Create(ADocumentModel);
  FParagraphFormattingInfoCache := CreateParagraphFormattingInfoCache(AUnitConverter);
  FParagraphFrameFormattingCache := TdxParagraphFrameFormattingCache.Create(ADocumentModel);
  FMergedParagraphFormattingInfoCache := CreateParagraphFormattingInfoCache(AUnitConverter);
  FParagraphFormattingCache := CreateParagraphFormattingCache(ADocumentModel);
  FInlinePictureInfoCache := TdxInlinePictureInfoCache.Create(AUnitConverter);
  FInlineCustomObjectInfoCache := TdxInlineCustomObjectInfoCache.Create(AUnitConverter);
  FUnitInfoCache := TdxWidthUnitInfoCache.Create(AUnitConverter);
  FBorderInfoCache := TdxBorderInfoCache.Create(AUnitConverter);
  FHeightUnitInfoCache := TdxHeightUnitInfoCache.Create(AUnitConverter);
  FTabFormattingInfoCache := TdxTabFormattingInfoCache.Create(AUnitConverter);
end;

function TdxSimpleDocumentCache.CreateParagraphFormattingCache(
  ADocumentModel: TdxCustomDocumentModel): TdxParagraphFormattingCache;
begin
  Result := TdxParagraphFormattingCache.Create(ADocumentModel);
end;

function TdxSimpleDocumentCache.CreateParagraphFormattingInfoCache(
  AUnitConverter: TdxDocumentModelUnitConverter): TdxParagraphFormattingInfoCache;
begin
  Result := TdxParagraphFormattingInfoCache.Create(AUnitConverter);
end;

end.
