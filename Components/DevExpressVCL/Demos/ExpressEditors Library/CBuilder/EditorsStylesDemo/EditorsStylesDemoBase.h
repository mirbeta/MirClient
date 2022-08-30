//---------------------------------------------------------------------------


#ifndef EditorsStylesDemoBaseH
#define EditorsStylesDemoBaseH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxMemo.hpp"
#include "cxPropertiesStore.hpp"
#include "cxTextEdit.hpp"
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "cxClasses.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
//---------------------------------------------------------------------------
enum TcxExtEditorType {eetLabel, eetProgressBar, eetTrackBar, eetCheckListBox,
    eetColorComboBox, eetFontNameComboBox, eetCheckComboBox, eetTreeView,
    eetShellTreeView, eetShellComboEdit, eetSplitter, eetGroupBox,
    eetSpinButton, eetHintStyleController, eetMCListBox, eetListView,
    eetHeader, eetShellListView, eetDBColorComboBox, eetDBLabel,
    eetDBProgressBar, eetDBTrackBar, eetDBCheckListBox, eetDBCheckComboBox,
    eetDBFontNameComboBox, eetDBShellComboEdit, eetMaskEdit, eetCheckBox,
    eetComboBox, eetButton, eetImage, eetDBTextEdit, eetDBSpinEdit,
    eetDBNavigator, eetDBDateEdit, eetDBLookupComboBox, eetDBMemo, eetGrid,
    eetCalcEdit, eetDateEdit, eetTextEdit, eetRichEdit, eetZoomTrackBar,
    eetCheckGroupBox, eetToggleSwitch};

typedef Set <TcxExtEditorType, eetLabel, eetCheckGroupBox> TcxExtEditorTypes;
enum TcxStyleSheetType {shtLightBlue, shtLightGray, shtWood, shtRainyDay,
   shtBrick, shtDeepSea};

enum TcxHintType {hcstLightInfo, hcstLightSlideLeft, hcstBlueSlideUp,
    hcstRoundedInfo, hcstStandard, hcstNoHint};

typedef void __fastcall (__closure *TcxFileNameChangedEvent)(String AFileName);

class TEditorsStylesDemoBaseFrame : public TForm
{
__published:	// IDE-managed Components
  TcxMemo *memDescrip;
  TPanel *pnlDescription;
  TcxEditStyleController *cxEditStyleController;
  TcxPropertiesStore *cxPropertiesStore;
  TcxEditStyleController *cxLabelStyleController;
  TTimer *FlickerTimer;
  void __fastcall FlickerTimerTimer(TObject *Sender);
  void __fastcall FrameResize(TObject *Sender);
private:	// User declarations
  TcxHintType FHintStyle;
  String FFlickeringClassName;
  TList* FFrameControls;
  TcxFileNameChangedEvent FOnFileNameChanged;
  void SetDisplayStyle(const TcxStyleSheetType Value);
  bool GetFlickering();
  void SetFlickering(const bool Value);
  void ClearFrameControls();
  void CreateFrameControls(TWinControl* AControl);
  void __fastcall cxSplitterMoved(TObject* Sender);
  void __fastcall cxSplitterCanResize(TObject* Sender, int &NewSize, bool &Accept);
  void AdjustFlickeringShapes(bool AIsFlickerRun);
protected:
  String FFileName;
  TcxStyleSheetType FDisplayStyle;
  TcxStyleSheetType FTempDisplayStyle;
  TColor FStyleBackgroundColor;
  bool FSplitterFlickering;
  void __fastcall DoOnFileNameChanged();
public:
  __fastcall TEditorsStylesDemoBaseFrame(TComponent* Owner);
  __fastcall ~TEditorsStylesDemoBaseFrame();
  TcxExtEditorTypes GetExtEditorTypes(TWinControl *AControl);
  virtual String __fastcall Name();
  virtual String __fastcall BriefName();
  virtual void ChangeDisplayStyle(TcxStyleSheetType ADisplayStyle);
  virtual TColor GetStyleBackgroundColor();
  virtual String Description();
  virtual bool ShowControlsAboveDescription();
  virtual void FlickerControls(String AControlClassName);
  virtual bool MenuOpenFileVisible();
  virtual bool MenuSaveFileVisible();
  virtual bool StyleMenuVisible();
  virtual String StylesIniPath();
  virtual void OpenFile(TObject *Sender);
  virtual void SaveFile(TObject *Sender);
  __property TcxHintType HintStyle = {read=FHintStyle, write=FHintStyle};
  __property TcxStyleSheetType DisplayStyle = {read=FDisplayStyle, write=SetDisplayStyle};
  __property String FileName = {read=FFileName};
  __property bool Flickering = {read=GetFlickering, write=SetFlickering};
  __property TcxFileNameChangedEvent OnFileNameChanged = {read=FOnFileNameChanged, write=FOnFileNameChanged};
};

class TEditorsStylesDemoFrameManager {
private:
  TList* FFrameList;
  TEditorsStylesDemoBaseFrame* GetFrame(int AIndex);
  int GetFramesCount();
  TEditorsStylesDemoBaseFrame* CreateFrameByID(int AID);
public:
  TEditorsStylesDemoFrameManager();
  ~TEditorsStylesDemoFrameManager();
  void AddFrame(TEditorsStylesDemoBaseFrame* AEditorsStylesDemoBaseFrame);
  __property int FramesCount = {read=GetFramesCount};
  __property TEditorsStylesDemoBaseFrame* Frames[int Index] = {read=GetFrame};
};

const String StyleSheetIniFiles[shtDeepSea+1] = {
    "StyleLightBlue.ini",
    "StyleLightGray.ini",
    "StyleWood.ini",
    "StyleRainyDay.ini",
    "StyleBrick.ini",
    "StyleDeepSea.ini"};

void cxGetEditorsNamesListByTypes(TStrings* AEditorsNames, TcxExtEditorTypes AExtEditorTypes);

TEditorsStylesDemoFrameManager* EditorsStylesDemoFrameManager();

//---------------------------------------------------------------------------
extern PACKAGE TEditorsStylesDemoBaseFrame *EditorsStylesDemoBaseFrame;
//---------------------------------------------------------------------------
#endif
