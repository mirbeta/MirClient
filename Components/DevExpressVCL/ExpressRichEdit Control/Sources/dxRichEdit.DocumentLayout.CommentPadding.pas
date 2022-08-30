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

unit dxRichEdit.DocumentLayout.CommentPadding;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, dxCoreClasses, cxClasses, dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core;

type

  TdxMoreButtonHorizontalAlignment = (
    Left,
    Right,
    Center
  );

  { TdxCommentPadding }

  TdxCommentPadding = record
  strict private
    FCommentLeft: Integer;
    FCommentRight: Integer;
    FContentLeft: Integer;
    FContentRight: Integer;
    FContentTop: Integer;
    FContentBottom: Integer;
    FDistanceBetweenComments: Integer;
    FMoreButtonOffsetX: Integer;
    FMoreButtonOffsetY: Integer;
    FMoreButtonHorizontalAlignment: TdxMoreButtonHorizontalAlignment;
    FMoreButtonSize: TSize;
  public
    constructor Create(ACommentLeft: Integer; ACommentRight: Integer; AContentLeft: Integer; AContentTop: Integer;
      AContentRight: Integer; AContentBottom: Integer; ADistanceBetweenComments: Integer; const AMoreButtonSize: TSize;
      AMoreButtonOffsetX: Integer; AMoreButtonOffsetY: Integer;
      AMoreButtonHorizontalAlignment: TdxMoreButtonHorizontalAlignment);
    class function GetDefaultCommentPadding(ADocumentModel: TdxCustomDocumentModel): TdxCommentPadding; static;

    property CommentLeft: Integer read FCommentLeft;
    property CommentRight: Integer read FCommentRight;
    property ContentLeft: Integer read FContentLeft;
    property ContentRight: Integer read FContentRight;
    property ContentTop: Integer read FContentTop;
    property ContentBottom: Integer read FContentBottom;
    property DistanceBetweenComments: Integer read FDistanceBetweenComments;
    property MoreButtonOffsetX: Integer read FMoreButtonOffsetX;
    property MoreButtonOffsetY: Integer read FMoreButtonOffsetY;
    property MoreButtonHorizontalAlignment: TdxMoreButtonHorizontalAlignment read FMoreButtonHorizontalAlignment;
    property MoreButtonSize: TSize read FMoreButtonSize;
  end;

implementation

uses
  dxRichEdit.DocumentLayout.UnitConverter, cxGeometry;

{ TdxCommentPadding }

constructor TdxCommentPadding.Create(ACommentLeft: Integer; ACommentRight: Integer;
  AContentLeft: Integer; AContentTop: Integer; AContentRight: Integer; AContentBottom: Integer;
  ADistanceBetweenComments: Integer; const AMoreButtonSize: TSize; AMoreButtonOffsetX: Integer;
  AMoreButtonOffsetY: Integer; AMoreButtonHorizontalAlignment: TdxMoreButtonHorizontalAlignment);
begin
  FCommentLeft := ACommentLeft;
  FCommentRight := ACommentRight;
  FContentLeft := AContentLeft;
  FContentTop := AContentTop;
  FContentRight := AContentRight;
  FContentBottom := AContentBottom;
  FDistanceBetweenComments := ADistanceBetweenComments;
  FMoreButtonOffsetX := AMoreButtonOffsetX;
  FMoreButtonOffsetY := AMoreButtonOffsetY;
  FMoreButtonHorizontalAlignment := AMoreButtonHorizontalAlignment;
  FMoreButtonSize := AMoreButtonSize;
end;

class function TdxCommentPadding.GetDefaultCommentPadding(ADocumentModel: TdxCustomDocumentModel): TdxCommentPadding;
var
  AConverter: TdxDocumentLayoutUnitConverter;
begin
  AConverter := ADocumentModel.LayoutUnitConverter;
  Result := TdxCommentPadding.Create(
  AConverter.DocumentsToLayoutUnits(112),
  AConverter.DocumentsToLayoutUnits(37),
    AConverter.DocumentsToLayoutUnits(20),
    AConverter.DocumentsToLayoutUnits(10),
    AConverter.DocumentsToLayoutUnits(20),
    AConverter.DocumentsToLayoutUnits(10),
    AConverter.DocumentsToLayoutUnits(6),
    cxSize(AConverter.PixelsToLayoutUnits(23, 96), AConverter.PixelsToLayoutUnits(14, 96)),
    0,
    0,
    TdxMoreButtonHorizontalAlignment.Right
  );
end;

end.
