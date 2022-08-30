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

unit dxRichEdit.DocumentModel.SectionRange;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.ParagraphRange;

type
  { TdxSectionRun }

  TdxSectionRun = class(TdxParagraphRun)
  public
    function CanJoinWith(ARun: TdxTextRunBase): Boolean; override;
    procedure Export(const AExporter: IdxSimpleDocumentModelExporter); override;
    procedure Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer); override;
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean; override;

    function CreateParagraphRun: TdxParagraphRun;
  end;

implementation

{ TdxSectionRun }

function TdxSectionRun.CanJoinWith(ARun: TdxTextRunBase): Boolean;
begin
  Result := False;
end;

procedure TdxSectionRun.Export(const AExporter: IdxSimpleDocumentModelExporter);
begin
  AExporter.Export(Self);
end;

function TdxSectionRun.CreateParagraphRun: TdxParagraphRun;
begin
  Result := TdxParagraphRun.Create(Paragraph);
  Result.StartIndex := StartIndex;
  Result.Length := Length;
  Result.InheritStyleAndFormattingFromCore(Self);
end;

procedure TdxSectionRun.Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer);
begin
  raise Exception.Create('TdxSectionRun.Measure');
end;

function TdxSectionRun.TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean;
begin
  Result := False;
end;

end.
