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
unit dxRichEdit.Import.Doc.DocCommandHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,

  dxRichEdit.Import.Doc.DCO;

type

  { TdxDocCommandHelper }

  TdxDocCommandHelper = class
  public const
    TablePropertiesOpcode = Word($646b);
    TableDefinitionOld    = Word($d606);
    TableDefinitionNew    = Word($d608);
    SprmPChgTabs          = Word($c615);
  public
    class function Traverse(const AGrpprl: TBytes; AFactory: TdxDocCommandFactoryBase; ADataStreamReader: TBinaryReader): TdxDocPropertyContainer; overload; static;
    class procedure Traverse(const AGrpprl: TBytes; AContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); overload; static;
    class function CreateDocCommands(const AGrpprl: TBytes; AFactory: TdxDocCommandFactoryBase; ADataStreamReader: TBinaryReader): TList<IdxDocCommand>; static;
    class function CreateSinglePropertyModifier(ASignedOpcode: SmallInt; const AParameters: TBytes): TBytes; static;
  end;

implementation

uses
  dxRichEdit.Import.Doc.DocImporter;

{ TdxDocCommandHelper }

class function TdxDocCommandHelper.Traverse(const AGrpprl: TBytes; AFactory: TdxDocCommandFactoryBase;
  ADataStreamReader: TBinaryReader): TdxDocPropertyContainer;
var
  ACommands: TList<IdxDocCommand>;
  ACount, I: Integer;
  AChangeActions: TdxChangeActionTypes;
begin
  ACommands := CreateDocCommands(AGrpprl, AFactory, ADataStreamReader);
  try
    ACount := ACommands.Count;
    AChangeActions := [];
    for I := 0 to ACount - 1 do
      Include(AChangeActions, ACommands[I].ChangeAction);

    Result := AFactory.CreatePropertyContainer(AChangeActions);
    for I := 0 to ACount - 1 do
      ACommands[I].Execute(Result, ADataStreamReader);
  finally
    ACommands.Free;
  end;
end;

class procedure TdxDocCommandHelper.Traverse(const AGrpprl: TBytes; AContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  ACommands: TList<IdxDocCommand>;
  ACount, I: Integer;
  AChangeActions: TdxChangeActionTypes;
begin
  ACommands := CreateDocCommands(AGrpprl, AContainer.Factory, ADataStreamReader);
  try
    ACount := ACommands.Count;
    AChangeActions := [];
    for I := 0 to ACount - 1 do
      Include(AChangeActions, ACommands[I].ChangeAction);
    AContainer.Update(AChangeActions);
    for I := 0 to ACount - 1 do
      ACommands[I].Execute(AContainer, ADataStreamReader);
  finally
    ACommands.Free;
  end;
end;

class function TdxDocCommandHelper.CreateDocCommands(const AGrpprl: TBytes; AFactory: TdxDocCommandFactoryBase;
  ADataStreamReader: TBinaryReader): TList<IdxDocCommand>;
var
  ASprm: TBytes;
  ASprmSize: SmallInt;
  ALength, AByteCounter, ASpra, AItbdDelMax, AItbdAddMaxOffset, AItbdAddMax: Integer;
  AOpcode: Word;
  ACommand: IdxDocCommand;
begin
  Result := TList<IdxDocCommand>.Create;
  ALength := Length(AGrpprl) - 2;
  AByteCounter := 0;
  while AByteCounter < ALength do
  begin
    AOpcode := PWord(@AGrpprl[AByteCounter])^;
    Inc(AByteCounter, 2);
    ASpra := AOpcode shr 13;
    case ASpra of
      0, 1:
        ASprmSize := 1;
      2, 4, 5:
        ASprmSize := 2;
      7:
        ASprmSize := 3;
      3:
        ASprmSize := 4;
      6:
        if (AOpcode = TableDefinitionOld) or (AOpcode = TableDefinitionNew) then
        begin
          ASprmSize := PSmallInt(@AGrpprl[AByteCounter])^ - 1;
          Inc(AByteCounter, 2);
        end
        else
          if AOpcode = SprmPChgTabs then
          begin
            ASprmSize := AGrpprl[AByteCounter];
            Inc(AByteCounter);
            if ASprmSize = 255 then
            begin
              AItbdDelMax := AGrpprl[AByteCounter];
              AItbdAddMaxOffset := AByteCounter + 1 + AItbdDelMax * 4 * 2;
              AItbdAddMax := AGrpprl[AItbdAddMaxOffset];
              ASprmSize := 2 + AItbdDelMax * 4 + AItbdAddMax * 3;
            end;
          end
          else
          begin
            ASprmSize := AGrpprl[AByteCounter];
            Inc(AByteCounter);
          end;
      else
      begin
        TdxDocImporter.ThrowInvalidDocFile;
        ASprmSize := -1;
      end;
    end;
    SetLength(ASprm, ASprmSize);
    if AByteCounter + Length(ASprm) > Length(AGrpprl) then
      Exit;
    Move(AGrpprl[AByteCounter], ASprm[0], Length(ASprm));
    Inc(AByteCounter, ASprmSize);

    ACommand := AFactory.CreateCommand(SmallInt(AOpcode));
    ACommand.Read(ASprm);
    Result.Add(ACommand);
    if AOpcode = TablePropertiesOpcode then
      Exit;
  end;
end;

class function TdxDocCommandHelper.CreateSinglePropertyModifier(ASignedOpcode: SmallInt; const AParameters: TBytes): TBytes;
var
  AOpcode: Word;
  AParametersLength: Integer;
begin
  AOpcode := Word(ASignedOpcode);
  AParametersLength := Length(AParameters);
  if AOpcode shr 13 <> 6 then
  begin
    SetLength(Result, SizeOf(Word) + AParametersLength);
    Move(AOpcode, Result[0], SizeOf(Word));
    Move(AParameters[0], Result[2], AParametersLength);
  end
  else
  begin
    SetLength(Result, SizeOf(Word) + AParametersLength + 1);
    Move(AOpcode, Result[0], SizeOf(Word));
    Result[2] := Byte(AParametersLength);
    Move(AParameters[0], Result[3], AParametersLength);
  end;
end;

end.
