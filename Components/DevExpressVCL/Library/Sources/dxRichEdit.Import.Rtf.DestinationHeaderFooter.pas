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

unit dxRichEdit.Import.Rtf.DestinationHeaderFooter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, dxCoreClasses,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.Import.Rtf,
  dxRichEdit.Import.Rtf.DestinationPieceTable;

type
  { TdxSectionHeaderFooterDestinationBase }

  TdxSectionHeaderFooterDestinationBase = class abstract(TdxDestinationPieceTable)
  strict private
    FSection: TdxSection;
  protected
    property Section: TdxSection read FSection;
  public
    constructor Create(AImporter: TdxRtfImporter; ASection: TdxSection;
      AHeaderFooter: TdxSectionHeaderFooterBase); reintroduce;
  end;

  { TdxSectionPageHeaderDestination }

  TdxSectionPageHeaderDestination = class(TdxSectionHeaderFooterDestinationBase)
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
  end;

  { TdxSectionPageFooterDestination }

  TdxSectionPageFooterDestination = class(TdxSectionHeaderFooterDestinationBase)
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
  end;

implementation

{ TdxSectionHeaderFooterDestinationBase }

constructor TdxSectionHeaderFooterDestinationBase.Create(
  AImporter: TdxRtfImporter; ASection: TdxSection;
  AHeaderFooter: TdxSectionHeaderFooterBase);
begin
  inherited Create(AImporter, TdxPieceTable(AHeaderFooter.PieceTable));
  FSection := ASection;
end;

{ TdxSectionPageHeaderDestination }

function TdxSectionPageHeaderDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxSectionPageHeaderDestination.Create(Importer, Section, TdxSectionHeader(PieceTable.ContentType));
end;

{ TdxSectionPageFooterDestination }

function TdxSectionPageFooterDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxSectionPageFooterDestination.Create(Importer, Section, TdxSectionHeader(PieceTable.ContentType));
end;

end.

