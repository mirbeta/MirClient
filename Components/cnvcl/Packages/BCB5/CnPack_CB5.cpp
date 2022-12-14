//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("CnPack_CB5.res");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("Vclx50.bpi");
USEPACKAGE("Vcldb50.bpi");
USEPACKAGE("Vclmid50.bpi");
USEPACKAGE("VclSmp50.bpi");
USEPACKAGE("vclado50.bpi");
USEUNIT("..\..\Source\Common\CnBase64.pas");
USEUNIT("..\..\Source\Common\CnCalendar.pas");
USEUNIT("..\..\Source\Common\CnClasses.pas");
USEUNIT("..\..\Source\Common\CnCommon.pas");
USEUNIT("..\..\Source\Common\CnConsts.pas");
USEUNIT("..\..\Source\Common\CnCRC32.pas");
USEUNIT("..\..\Source\Common\CnGraphUtils.pas");
USEUNIT("..\..\Source\Common\CnHashMap.pas");
USEUNIT("..\..\Source\Common\CnIni.pas");
USEUNIT("..\..\Source\Common\CnIniStrUtils.pas");
USEUNIT("..\..\Source\Common\CnMD5.pas");
USEUNIT("..\..\Source\Common\CnSingleton.pas");
USEUNIT("..\..\Source\Common\CnSingletonComp.pas");
USEUNIT("..\..\Source\Common\CnPack.pas");
USERES("..\..\Source\Common\CnPack.dcr");
USEUNIT("..\..\Source\Common\CnSQLite.pas");
USEUNIT("..\..\Source\Common\CnStrDiff.pas");
USEUNIT("..\..\Source\Common\CnStream.pas");
USEUNIT("..\..\Source\Common\CnTree.pas");
USEUNIT("..\..\Source\Common\CnVCLBase.pas");
USEUNIT("..\..\Source\Common\CnWinSvc.pas");
USEUNIT("..\..\Source\DbReport\CnDBConsts.pas");
USEUNIT("..\..\Source\DbReport\CnADOUpdateSQL.pas");
USEUNIT("..\..\Source\DbReport\CnPagedGrid.pas");
USEUNIT("..\..\Source\Graphics\CnAOTreeView.pas");
USEUNIT("..\..\Source\Graphics\CnAutoOption.pas");
USEUNIT("..\..\Source\Graphics\CnCheckTreeView.pas");
USEUNIT("..\..\Source\Graphics\CnEdit.pas");
USEUNIT("..\..\Source\Graphics\CnListBox.pas");
USEUNIT("..\..\Source\Graphics\CnGraphConsts.pas");
USEUNIT("..\..\Source\Graphics\CnGraphics.pas");
USEUNIT("..\..\Source\Graphics\CnImage.pas");
USEUNIT("..\..\Source\Graphics\CnShellCtrls.pas");
USEUNIT("..\..\Source\Graphics\CnSpin.pas");
USEUNIT("..\..\Source\Graphics\CnWizardImage.pas");
USEUNIT("..\..\Source\Graphics\CnButtons.pas");
USEUNIT("..\..\Source\Graphics\CnHexEditor.pas");
USEUNIT("..\..\Source\Graphics\CnGauge.pas");
USEUNIT("..\..\Source\Graphics\CnMonthCalendar.pas");
USEUNIT("..\..\Source\Graphics\CnColorGrid.pas");
USEUNIT("..\..\Source\Graphics\CnErrorProvider.pas");
USEUNIT("..\..\Source\Graphics\CnLED.pas");
USEUNIT("..\..\Source\MultiLang\CnHashIniFile.pas");
USEUNIT("..\..\Source\MultiLang\CnHashLangStorage.pas");
USEUNIT("..\..\Source\MultiLang\CnIniLangFileStorage.pas");
USEUNIT("..\..\Source\MultiLang\CnLangCollection.pas");
USEUNIT("..\..\Source\MultiLang\CnLangConsts.pas");
USEUNIT("..\..\Source\MultiLang\CnLangMgr.pas");
USEUNIT("..\..\Source\MultiLang\CnLangStorage.pas");
USEUNIT("..\..\Source\MultiLang\CnLangTranslator.pas");
USEUNIT("..\..\Source\NetComm\CnModem.pas");
USEUNIT("..\..\Source\NetComm\CnNetConsts.pas");
USEUNIT("..\..\Source\NetComm\CnRS232.pas");
USEUNIT("..\..\Source\NetComm\CnRS232Dialog.pas");
USEUNIT("..\..\Source\NetComm\CnIP.pas");
USEUNIT("..\..\Source\NetComm\CnPing.pas");
USEUNIT("..\..\Source\NetComm\CnDialUp.pas");
USEUNIT("..\..\Source\NetComm\CnCameraEye.pas");
USEUNIT("..\..\Source\NetComm\CnIISCtrl.pas");
USEUNIT("..\..\Source\NetComm\CnTwain.pas");
USEUNIT("..\..\Source\NetComm\CnIocpSimpleMemPool.pas");
USEUNIT("..\..\Source\NetComm\CnIocpSocketAdapter.pas");
USEUNIT("..\..\Source\NonVisual\CnActionListHook.pas");
USEUNIT("..\..\Source\NonVisual\CnActiveScript.pas");
USEUNIT("..\..\Source\NonVisual\CnADOConPool.pas");
USEUNIT("..\..\Source\NonVisual\CnCompConsts.pas");
USEUNIT("..\..\Source\NonVisual\CnControlHook.pas");
USEUNIT("..\..\Source\NonVisual\CnDragResizer.pas");
USEUNIT("..\..\Source\NonVisual\CnFormScaler.pas");
USEUNIT("..\..\Source\NonVisual\CnKeyBlocker.pas");
USEUNIT("..\..\Source\NonVisual\CnMDIBackGround.pas");
USEUNIT("..\..\Source\NonVisual\CnMenuHook.pas");
USEUNIT("..\..\Source\NonVisual\CnObjectPool.pas");
USEUNIT("..\..\Source\NonVisual\CnRawInput.pas");
USEUNIT("..\..\Source\NonVisual\CnRestoreSystemMenu.pas");
USEUNIT("..\..\Source\NonVisual\CnSystemDebugControl.pas");
USEUNIT("..\..\Source\NonVisual\CnTaskBar.pas");
USEUNIT("..\..\Source\NonVisual\CnThreadPool.pas");
USEUNIT("..\..\Source\NonVisual\CnTimer.pas");
USEUNIT("..\..\Source\NonVisual\CnTrayIcon.pas");
USEUNIT("..\..\Source\NonVisual\CnVolumeCtrl.pas");
USEUNIT("..\..\Source\NonVisual\CnWinampCtrl.pas");
USEUNIT("..\..\Source\Graphics\CnValidateImage.pas");
USEUNIT("..\..\Source\Graphics\CnWaterEffect.pas");
USEUNIT("..\..\Source\Graphics\CnWaterImage.pas");
USEUNIT("..\..\Source\Graphics\CnAACtrls.pas");
USEUNIT("..\..\Source\Graphics\CnAAFont.pas");
USEUNIT("..\..\Source\Graphics\CnTabSet.pas");
USEUNIT("..\..\Source\Graphics\CnButtonEdit.pas");
USEUNIT("..\..\Source\Graphics\CnPanel.pas");
USEUNIT("..\..\Source\Graphics\CnAAFontDialog.pas");
USEUNIT("..\..\Source\Graphics\CnSkinMagic.pas");
USEUNIT("..\..\Source\Graphics\CnHint.pas");
USEUNIT("..\..\Source\Graphics\CnQQPanel.pas");
USEUNIT("..\..\Source\Graphics\CnAppStoreBox.pas");
USEUNIT("..\..\Source\NonVisual\CnConsole.pas");
USEUNIT("..\..\Source\NonVisual\CnFilePacker.pas");
USEUNIT("..\..\Source\NonVisual\CnFileSystemWatcher.pas");
USEUNIT("..\..\Source\NonVisual\CnOuterControls.pas");
USEUNIT("..\..\Source\NonVisual\CnGlobalKeyHook.pas");
USEUNIT("..\..\Source\NonVisual\CnDockInfo.pas");
USEUNIT("..\..\Source\NonVisual\CnDockSupportProc.pas");
USEUNIT("..\..\Source\NonVisual\CnDockFormControl.pas");
USEUNIT("..\..\Source\NonVisual\CnDockSupportControl.pas");
USEUNIT("..\..\Source\NonVisual\CnDockSupportClass.pas");
USEUNIT("..\..\Source\NonVisual\CnDockGlobal.pas");
USEUNIT("..\..\Source\NonVisual\CnVSNETDockStyle.pas");
USEUNIT("..\..\Source\NonVisual\CnVIDDockStyle.pas");
USEUNIT("..\..\Source\NonVisual\CnVCDockStyle.pas");
USEUNIT("..\..\Source\NonVisual\CnDelphiDockStyle.pas");
USEUNIT("..\..\Source\NonVisual\CnDockTree.pas");
USEUNIT("..\..\Source\NonVisual\CnDockHashTable.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
