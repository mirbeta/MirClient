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

unit dxRichEdit.Import.Rtf.DestinationColorTable;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Graphics, Windows, Generics.Defaults, Generics.Collections,
  dxCoreClasses, dxCoreGraphics,
  dxRichEdit.Import.Rtf;

type

  { TdxColorTableDestination }

  TdxColorTableDestination = class(TdxRichEditRtfDestinationBase)
  strict private const
    AutoColor = TdxAlphaColors.Empty;
  strict private class var
    FDefaultMSWordColor: array[0..17] of TdxAlphaColor;
  strict private
    class constructor Initialize;
  strict private
    R: Integer;
    G: Integer;
    B: Integer;
    FWasColor: Boolean;
    function IsColorValid: Boolean;
    procedure Reset;
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    function ProcessKeywordCore(const AKeyword: string; AParameterValue: Integer;
      AHasParameter: Boolean): Boolean; override;
    procedure ProcessCharCore(AChar: Char); override;
  end;

implementation

uses
  dxCore,
  dxGenerics;

{ TdxColorTableDestination }

function TdxColorTableDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxColorTableDestination.Create(Importer);
end;

class constructor TdxColorTableDestination.Initialize;
begin
  FDefaultMSWordColor[0]  := AutoColor;
  FDefaultMSWordColor[1]  := TdxAlphaColors.FromArgb(255, 0, 0, 0);
  FDefaultMSWordColor[2]  := TdxAlphaColors.FromArgb(255, 0, 0, 255);
  FDefaultMSWordColor[3]  := TdxAlphaColors.FromArgb(255, 0, 255, 255);
  FDefaultMSWordColor[4]  := TdxAlphaColors.FromArgb(255, 0, 255, 0);
  FDefaultMSWordColor[5]  := TdxAlphaColors.FromArgb(255, 255, 0, 255);
  FDefaultMSWordColor[6]  := TdxAlphaColors.FromArgb(255, 255, 0, 0);
  FDefaultMSWordColor[7]  := TdxAlphaColors.FromArgb(255, 255, 255, 0);
  FDefaultMSWordColor[8]  := TdxAlphaColors.FromArgb(255, 255, 255, 255);
  FDefaultMSWordColor[9]  := TdxAlphaColors.FromArgb(255, 0, 0, 128);
  FDefaultMSWordColor[10] := TdxAlphaColors.FromArgb(255, 0, 128, 128);
  FDefaultMSWordColor[11] := TdxAlphaColors.FromArgb(255, 0, 128, 0);
  FDefaultMSWordColor[12] := TdxAlphaColors.FromArgb(255, 128, 0, 128);
  FDefaultMSWordColor[13] := TdxAlphaColors.FromArgb(255, 128, 0, 0);
  FDefaultMSWordColor[14] := TdxAlphaColors.FromArgb(255, 128, 128, 0);
  FDefaultMSWordColor[15] := TdxAlphaColors.FromArgb(255, 128, 128, 128);
  FDefaultMSWordColor[16] := TdxAlphaColors.FromArgb(255, 192, 192, 192);
end;

procedure TdxColorTableDestination.Reset;
begin
  FWasColor := False;
end;

function TdxColorTableDestination.ProcessKeywordCore(const AKeyword: string;
  AParameterValue: Integer; AHasParameter: Boolean): Boolean;
begin
  if not AHasParameter  then
    AParameterValue := 0;
  Result := True;
  if AKeyword = 'bin' then
    Result := inherited ProcessKeywordCore(AKeyword, AParameterValue, AHasParameter)
  else
    if AKeyword = 'red' then
    begin
      R := AParameterValue;
      FWasColor := True;
    end
    else
      if AKeyword = 'green' then
      begin
        G := AParameterValue;
        FWasColor := True;
      end
      else
        if AKeyword = 'blue' then
        begin
          B := AParameterValue;
          FWasColor := True;
        end
        else
          Result := False;
end;

function TdxColorTableDestination.IsColorValid: Boolean;

  function IsValid(Value: Integer): Boolean;
  begin
    Result := (Value >= 0) and (Value <= 255);
  end;

begin
  Result := IsValid(R) and IsValid(G) and IsValid(B);
end;

procedure TdxColorTableDestination.ProcessCharCore(AChar: Char);
var
  AColor: TdxAlphaColor;
  ANewColorIndex: Integer;
  AColors: TdxAlphaColorList;
begin
  if AChar = ';' then
  begin
    if FWasColor then
    begin
      if IsColorValid then
        Importer.DocumentProperties.Colors.Add(TdxAlphaColors.FromArgb(R, G, B))
      else
        TdxRtfImporter.ThrowInvalidRtfFile;
    end
    else
    begin
      AColors := Importer.DocumentProperties.Colors;
      ANewColorIndex := AColors.Count;
      if ANewColorIndex < Length(FDefaultMSWordColor) then
      begin
        AColor := FDefaultMSWordColor[ANewColorIndex];
        AColors.Add(AColor);
      end;
    end;
    Reset;
  end;
end;

end.
