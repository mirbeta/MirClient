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

unit dxRichEdit.Import.Rtf.DestinationPageBackground;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,

  dxRichEdit.Import.Rtf;

type
  { TdxPageBackgroundDestination }

  TdxPageBackgroundDestination = class(TdxRichEditRtfDestinationBase)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  protected
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    class function GetControlCharHT: TdxControlCharTranslatorTable; override;
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class procedure ShapeHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    procedure BeforeNestedGroupFinishedCore(ANestedDestination: TdxRichEditRtfDestinationBase); override;
  end;

implementation

uses
  dxRichEdit.Utils.Types,
  dxRichEdit.Import.Rtf.DestinationShape;

type
  TdxShapeInstanceDestinationAccess = class(TdxShapeInstanceDestination);

{ TdxPageBackgroundDestination }

class constructor TdxPageBackgroundDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxPageBackgroundDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxPageBackgroundDestination.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('shp', ShapeHandler);
end;

class function TdxPageBackgroundDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class function TdxPageBackgroundDestination.GetControlCharHT: TdxControlCharTranslatorTable;
begin
  Result := nil;
end;

function TdxPageBackgroundDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxPageBackgroundDestination.Create(Importer);
end;

class procedure TdxPageBackgroundDestination.ShapeHandler(AImporter: TdxRtfImporter;
  AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxShapeInstanceDestination.Create(AImporter);
end;

procedure TdxPageBackgroundDestination.BeforeNestedGroupFinishedCore(ANestedDestination: TdxRichEditRtfDestinationBase);
var
  ADestination: TdxShapeInstanceDestination;
begin
  inherited BeforeNestedGroupFinishedCore(ANestedDestination);
  ADestination := Safe<TdxShapeInstanceDestination>.Cast(Importer.Destination);
  if ADestination = nil then
    Exit;

  if TdxShapeInstanceDestinationAccess(ADestination).HasColorProperty('fillColor') then
    Importer.DocumentModel.DocumentProperties.PageBackColor := TdxShapeInstanceDestinationAccess(ADestination).GetColorPropertyValue('fillColor');
end;

end.
