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

unit dxRichEdit.Import.Rtf.DestinationSkip;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, dxCoreClasses, dxRichEdit.Import.Rtf;

type
  TdxSkipDestination = class(TdxRichEditRtfDestinationBase)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  private
    FOldDecoder: TdxCodePageCharacterDecoder;
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;

    procedure ProcessControlCharCore(AChar: Char); override;
    function ProcessKeywordCore(const AKeyword: string; AParameterValue: Integer; AHasParameter: Boolean): Boolean; override;
    procedure ProcessCharCore(AChar: Char); override;
  public
    constructor Create(AImporter: TdxRtfImporter); override;
    destructor Destroy; override;
    procedure BeforePopRtfState; override;
  end;

  { TdxSkipNestedTableDestination }

  TdxSkipNestedTableDestination = class(TdxSkipDestination)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  private
    class procedure ParKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
  end;

  { TdxSkipCharacterDecoder }

  TdxSkipCharacterDecoder = class(TdxCodePageCharacterDecoder)
  public
    constructor Create; reintroduce;
    procedure ProcessChar(AImporter: TdxRtfImporter; AChar: Char); override;
    procedure Flush(AImporter: TdxRtfImporter); override;
  end;

implementation

uses
  dxEncoding;

type
  TdxRtfFormattingInfoAccess = class(TdxRtfFormattingInfo);

{ TdxSkipDestination }

constructor TdxSkipDestination.Create(AImporter: TdxRtfImporter);
begin
  inherited Create(AImporter);
  FOldDecoder := Importer.Position.RtfFormattingInfo.Decoder;
  TdxRtfFormattingInfoAccess(Importer.Position.RtfFormattingInfo).SetDecoder(TdxSkipCharacterDecoder.Create, False);
end;

function TdxSkipDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxSkipDestination.Create(Importer);
end;

destructor TdxSkipDestination.Destroy;
begin
  FreeAndNil(FOldDecoder);
  inherited Destroy;
end;

class constructor TdxSkipDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxSkipDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxSkipDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

procedure TdxSkipDestination.BeforePopRtfState;
begin
  TdxRtfFormattingInfoAccess(Importer.Position.RtfFormattingInfo).SetDecoder(FOldDecoder);
  FOldDecoder := nil;
  inherited BeforePopRtfState;
end;

class function TdxSkipDestination.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('bin', BinKeywordHandler);
end;

procedure TdxSkipDestination.ProcessControlCharCore(AChar: Char);
begin
end;

function TdxSkipDestination.ProcessKeywordCore(const AKeyword: string;
  AParameterValue: Integer; AHasParameter: Boolean): Boolean;
var
  ATranslator: TdxTranslateKeywordEvent;
begin
  Result := KeywordHT.TryGetValue(AKeyword, ATranslator);
  if Result then
    ATranslator(Importer, AParameterValue, AHasParameter);
end;

procedure TdxSkipDestination.ProcessCharCore(AChar: Char);
begin
end;

{ TdxSkipNestedTableDestination }

function TdxSkipNestedTableDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxSkipNestedTableDestination.Create(Importer);
end;

class function TdxSkipNestedTableDestination.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('par', ParKeywordHandler);
end;

class destructor TdxSkipNestedTableDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxSkipNestedTableDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class constructor TdxSkipNestedTableDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class procedure TdxSkipNestedTableDestination.ParKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := AImporter.StateManager.CreateDefaultDestination;
end;

{ TdxSkipCharacterDecoder }

constructor TdxSkipCharacterDecoder.Create;
begin
  inherited Create(TEncoding.Default.CodePage);
end;

procedure TdxSkipCharacterDecoder.Flush(AImporter: TdxRtfImporter);
begin
//do nothing
end;

procedure TdxSkipCharacterDecoder.ProcessChar(AImporter: TdxRtfImporter;
  AChar: Char);
begin
//do nothing
end;

end.


