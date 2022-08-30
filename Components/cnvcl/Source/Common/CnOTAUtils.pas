{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2010 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnOTAUtils;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ�OTA ����ڹ��ߵ�Ԫ�������� CnWizUtils
* ��Ԫ���ߣ�CnPack ������ ��Х (liuxiao@cnpack.org)
* ��    ע���õ�Ԫʵ����һЩ����ڵ� OTA ��غ���
* ����ƽ̨��PWinXP + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��Ԫ��ʶ��$Id: CnOTAUtils.pas 418 2010-02-08 04:53:54Z zhoujingyu $
* �޸ļ�¼��2006.08.19 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, ToolsAPI,
  {$IFDEF COMPILER6_UP}Variants, {$ENDIF}
  CnCommon;
  
function CnOtaGetProjectGroup: IOTAProjectGroup;
{* ȡ��ǰ������ }

function CnOtaGetCurrentProject: IOTAProject;
{* ȡ��ǰ���� }

function CnOtaGetCurrentProjectFileName: string;
{* ȡ��ǰ�����ļ����� }

function CnOtaGetActiveProjectOptions(Project: IOTAProject = nil): IOTAProjectOptions;
{* ȡ��ǰ����ѡ�� }

function CnOtaGetActiveProjectOption(const Option: string; var Value: Variant): Boolean;
{* ȡ��ǰ����ָ��ѡ�� }

function CnOtaGetOutputDir: string;
{* ȡ��ǰ�������Ŀ¼ }

function CnOtaGetFileNameOfModule(Module: IOTAModule;
  GetSourceEditorFileName: Boolean = False): string;
{* ȡָ��ģ���ļ�����GetSourceEditorFileName ��ʾ�Ƿ񷵻��ڴ���༭���д򿪵��ļ�}

function CnOtaGetFileNameOfCurrentModule(GetSourceEditorFileName: Boolean = False): string;
{* ȡ��ǰģ���ļ���}

function CnOtaGetCurrentModule: IOTAModule;
{* ȡ��ǰģ��}

function CnOtaIsFileOpen(const FileName: string): Boolean;
{* �ж��ļ��Ƿ�� }

function IsCpp(const FileName: string): Boolean;
{* �ж��Ƿ�.Cpp�ļ�}

implementation

{ Other DesignTime Utils Routines }

// ȡ��ǰ������
function CnOtaGetProjectGroup: IOTAProjectGroup;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  i: Integer;
begin
  Result := nil;
  Supports(BorlandIDEServices, IOTAModuleServices, IModuleServices);
  if IModuleServices <> nil then
    for i := 0 to IModuleServices.ModuleCount - 1 do
    begin
      IModule := IModuleServices.Modules[i];
      if Supports(IModule, IOTAProjectGroup, Result) then
        Break;
    end;
end;

// ȡ��ǰ����
function CnOtaGetCurrentProject: IOTAProject;
var
  IProjectGroup: IOTAProjectGroup;
begin
  Result := nil;

  IProjectGroup := CnOtaGetProjectGroup;
  if not Assigned(IProjectGroup) then
    Exit;

  try
    Result := IProjectGroup.ActiveProject;
  except
    Result := nil;
  end;
end;

// ȡ��ǰ�����ļ�����
function CnOtaGetCurrentProjectFileName: string;
var
  CurrentProject: IOTAProject;
begin
  CurrentProject := CnOtaGetCurrentProject;
  if Assigned(CurrentProject) then
    Result := CurrentProject.FileName
  else
    Result := '';
end;

// ȡ��ǰ����ѡ��
function CnOtaGetActiveProjectOptions(Project: IOTAProject = nil): IOTAProjectOptions;
begin
  Result := nil;
  if Assigned(Project) then
  begin
    Result:=Project.ProjectOptions;
    Exit;
  end;

  Project := CnOtaGetCurrentProject;
  if Assigned(Project) then
    Result := Project.ProjectOptions;
end;

// ȡ��ǰ����ָ��ѡ��
function CnOtaGetActiveProjectOption(const Option: string; var Value: Variant): Boolean;
var
  ProjectOptions: IOTAProjectOptions;
begin
  Result := False;
  Value := '';
  ProjectOptions := CnOtaGetActiveProjectOptions;
  if Assigned(ProjectOptions) then
  begin
    Value := ProjectOptions.Values[Option];
    Result := True;
  end;
end;

// ȡ��ǰ�������Ŀ¼
function CnOtaGetOutputDir: string;
var
  ProjectDir: string;
  OutputDir: Variant;
begin
  ProjectDir := ExtractFileDir(CnOtaGetCurrentProjectFileName);
  if CnOtaGetActiveProjectOption('OutputDir', OutputDir) then
    Result := LinkPath(ProjectDir, OutputDir)
  else
    Result := ProjectDir;
end;

// ȡָ��ģ���ļ�����GetSourceEditorFileName ��ʾ�Ƿ񷵻��ڴ���༭���д򿪵��ļ�
function CnOtaGetFileNameOfModule(Module: IOTAModule;
  GetSourceEditorFileName: Boolean): string;
var
  i: Integer;
  Editor: IOTAEditor;
  SourceEditor: IOTASourceEditor;
begin
  Result := '';
  if Assigned(Module) then
    if not GetSourceEditorFileName then
      Result := Module.FileName
    else
      for i := 0 to Module.GetModuleFileCount - 1 do
      begin
        Editor := Module.GetModuleFileEditor(i);
        if Supports(Editor, IOTASourceEditor, SourceEditor) then
        begin
          Result := Editor.FileName;
          Break;
        end;
      end;
end;

// ȡ��ǰģ���ļ���
function CnOtaGetFileNameOfCurrentModule(GetSourceEditorFileName: Boolean): string;
begin
  Result := CnOtaGetFileNameOfModule(CnOtaGetCurrentModule, GetSourceEditorFileName);
end;

// ȡ��ǰģ��
function CnOtaGetCurrentModule: IOTAModule;
var
  iModuleServices: IOTAModuleServices;
begin
  Result := nil;
  Supports(BorlandIDEServices, IOTAModuleServices, iModuleServices);
  if iModuleServices <> nil then
    Result := iModuleServices.CurrentModule;
end;

// ȡģ��༭��
function CnOtaGetFileEditorForModule(Module: IOTAModule; Index: Integer): IOTAEditor;
begin
  Result := nil;
  if not Assigned(Module) then Exit;
  try
    // BCB 5 ��Ϊһ���򵥵ĵ�Ԫ���� GetModuleFileEditor(1) �����
    {$IFDEF BCB5}
    if IsCpp(Module.FileName) and (Module.GetModuleFileCount = 2) and (Index = 1) then
      Index := 2;
    {$ENDIF}
    Result := Module.GetModuleFileEditor(Index);
  except
    Result := nil; // �� IDE �ͷ�ʱ�����ܻ����쳣����
  end;
end;

// �ж��ļ��Ƿ��
function CnOtaIsFileOpen(const FileName: string): Boolean;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  FileEditor: IOTAEditor;
  i: Integer;
begin
  Result := False;

  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  if ModuleServices = nil then Exit;

  Module := ModuleServices.FindModule(FileName);
  if Assigned(Module) then
  begin
    for i := 0 to Module.GetModuleFileCount-1 do
    begin
      FileEditor := CnOtaGetFileEditorForModule(Module, i);
      Assert(Assigned(FileEditor));

      Result := CompareText(FileName, FileEditor.FileName) = 0;
      if Result then
        Exit;
    end;
  end;
end;

// �ж��Ƿ�.Cpp�ļ�
function IsCpp(const FileName: string): Boolean;
var
  FileExt: string;
begin
  FileExt := UpperCase(ExtractFileExt(FileName));
  Result := (FileExt = '.CPP');
end;

end.
