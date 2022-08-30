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
unit dxRichEdit.Export.Doc.SectionPropertiesWriter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.Import.Doc.SectionPropertiesHelper;

type

  { TdxSectionPropertiesWriter }

  TdxSectionPropertiesWriter = class
  strict private
    FWriter: TBinaryWriter;
    FSectionHelper: TdxSectionPropertiesHelper;
  protected
    function GetSectionGroupPropertiesModifiers(ASection: TdxSection): TBytes;
  public
    constructor Create(AMemoryStream: TdxMemoryStream);
    destructor Destroy; override;
    procedure WriteSection(ACharacterPosition: Integer; ASection: TdxSection);
    procedure Finish(ALastPosition: Integer);

    property SectionHelper: TdxSectionPropertiesHelper read FSectionHelper;
  end;

implementation

uses
  Math, Contnrs,
  dxRichEdit.Export.Doc.DocSectionPropertiesActions;

{ TdxSectionPropertiesWriter }

constructor TdxSectionPropertiesWriter.Create(AMemoryStream: TdxMemoryStream);
begin
  Assert(AMemoryStream <> nil, 'memoryStream');
  FWriter := TBinaryWriter.Create(AMemoryStream);
  FSectionHelper := TdxSectionPropertiesHelper.Create;
end;

destructor TdxSectionPropertiesWriter.Destroy;
begin
  FSectionHelper.Free;
  FWriter.Free;
  inherited Destroy;
end;

procedure TdxSectionPropertiesWriter.WriteSection(ACharacterPosition: Integer; ASection: TdxSection);
var
  AGrpprl: TBytes;
  ASepxOffset: Integer;
begin
  AGrpprl := GetSectionGroupPropertiesModifiers(ASection);
  if Length(AGrpprl) = 0 then
    FSectionHelper.AddEntry(ACharacterPosition, -1)
  else
  begin
    ASepxOffset := Integer(FWriter.BaseStream.Position);
    FWriter.Write(Word(Length(AGrpprl)));
    FWriter.Write(AGrpprl);
    FSectionHelper.AddEntry(ACharacterPosition, ASepxOffset);
  end;
end;

procedure TdxSectionPropertiesWriter.Finish(ALastPosition: Integer);
begin
  FSectionHelper.AddLastPosition(ALastPosition);
end;

function TdxSectionPropertiesWriter.GetSectionGroupPropertiesModifiers(ASection: TdxSection): TBytes;
var
  AOutput: TdxMemoryStream;
  AActions: TdxDocSectionPropertiesActions;
begin
  AOutput := TdxMemoryStream.Create;
  try
    AActions := TdxDocSectionPropertiesActions.Create(AOutput, ASection);
    try
      AActions.CreateSectionPropertiesModifiers;
      Result := AOutput.ToArray;
    finally
      AActions.Free;
    end;
  finally
    AOutput.Free;
  end;
end;

end.
