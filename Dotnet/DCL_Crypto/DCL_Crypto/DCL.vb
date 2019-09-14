Public Class DCL

    Private Shared textPlainAscCode, signatureAscCode, matProcessData, alphaProcessData, sumAlphaprocessItems, lenAlphaProcessItems As New ArrayList
        Private Shared nListConvert, charListCode, numCharListCode, numCharListCodeSenTwo As New ArrayList
        Private Shared alpha As UInt16 = 1
        Private Shared debugOt As Boolean = True
        Private Shared hash As HASHINF
        Private Structure HASHINF
            Dim plainText As String
            Dim key As String
            Dim alpha As UInt16
            Dim len As UInt16
            Dim LPNStr As String
            Dim result As String
        End Structure
        ''' <summary>
        ''' آماده سازی کاراکتر های سالت
        ''' </summary>
        Public Sub New()
            charListCode.Add("A")
            numCharListCode.Add(0)
            charListCode.Add("B")
            numCharListCode.Add(1)
            charListCode.Add("C")
            numCharListCode.Add(2)
            charListCode.Add("D")
            numCharListCode.Add(3)
            charListCode.Add("E")
            numCharListCode.Add(4)
            charListCode.Add("F")
            numCharListCode.Add(5)
            charListCode.Add("Z")
            numCharListCode.Add(6)
            charListCode.Add("X")
            numCharListCode.Add(7)
            charListCode.Add("Y")
            numCharListCode.Add(8)
            charListCode.Add("S")
            numCharListCode.Add(9)

            numCharListCodeSenTwo.Add(9)
            numCharListCodeSenTwo.Add(8)
            numCharListCodeSenTwo.Add(7)
            numCharListCodeSenTwo.Add(6)
            numCharListCodeSenTwo.Add(5)
            numCharListCodeSenTwo.Add(4)
            numCharListCodeSenTwo.Add(3)
            numCharListCodeSenTwo.Add(2)
            numCharListCodeSenTwo.Add(1)
            numCharListCodeSenTwo.Add(0)

        End Sub

        ''' <summary>
        ''' پاک کردن و تازه سازی لیست ها برای تولید یک هش تازه
        ''' </summary>
        Private Shared Sub ClearData()
            If textPlainAscCode.Count > 0 Then
                textPlainAscCode.Clear()
                signatureAscCode.Clear()
                matProcessData.Clear()
                alphaProcessData.Clear()
                sumAlphaprocessItems.Clear()
                lenAlphaProcessItems.Clear()
                nListConvert.Clear()
                hash = New HASHINF
            End If
        End Sub

        ''' <summary>
        ''' در صورتی که آلفا ورودی 0 باشد ، با این تابع با توجه به کلید یک آلفای جدید تولید می شود
        ''' </summary>
        ''' <param name="alpha"></param>
        ''' <param name="key"></param>
        Private Shared Sub MakeNewAlpha(ByRef alpha As UInt16, key As String)
            Dim getCharOfkey As Char = key(0)
            Dim getAsciiOfChar As UInt16 = Asc(getCharOfkey)
        alpha = getAsciiOfChar.ToString()(getAsciiOfChar.ToString.Length - 1).ToString
        If alpha = 0 Then alpha = 5
    End Sub

        ''' <summary>
        ''' اعتبار سنجی تکست ورودی ، کلید و آلفا در این تابع انجام می گیرد
        ''' </summary>
        ''' <param name="text"></param>
        ''' <param name="key"></param>
        ''' <param name="alpha"></param>
        Private Shared Sub CheckInput(text As String, key As String, alpha As UInt16)
            If text = Nothing Then
                Throw New ArgumentException("The input can not be empty for hashing.")
            ElseIf key = Nothing Then
                Throw New ArgumentException("Invaild empty [ NULL ] key.")
            ElseIf alpha > 10 Then
                Throw New ArgumentException("Alpha should be smaller than or equal to 10.")
            End If
            Return
        End Sub
        ''' <summary>
        ''' این تابع وظیفه فراخانی توابع اساسی و زیرلایه ای دی سی ال را دارد
        ''' </summary>
        ''' <param name="text"></param>
        ''' <param name="key"></param>
        ''' <param name="alpha"></param>
        ''' <returns></returns>
        Public Function GetHash(text As String, key As String, alpha As UInt16) As String
            ClearData()
            CheckInput(text, key, alpha)
            If alpha = 0 Then MakeNewAlpha(alpha, key)
            hash.plainText = text
            hash.len = text.Length
            hash.key = key
            hash.alpha = alpha
            GetAscListCode(text, textPlainAscCode, False)
            GetAscListCode(key, signatureAscCode, False)
            MatProcess()
            AlphaProcess()
            GetRend()
            LPNProcess()
            Return hash.result
        End Function

        Private Shared Sub GetRend()
            Dim getLen, getSum As Int16
            Dim getCharNum As String
            Dim getItemAlpha As String = String.Empty
            For index = 0 To alphaProcessData.Count - 1
                getItemAlpha = alphaProcessData(index)

                If getItemAlpha.StartsWith(Chr(45)) Then
                    getItemAlpha = getItemAlpha.Remove(0, 1)
                End If
                If getItemAlpha.Contains(Chr(46)) Then
                    getItemAlpha = getItemAlpha.Remove(getItemAlpha.IndexOf(Chr(46)))
                End If

                getLen = getItemAlpha.Length

                For iSum = 0 To getLen - 1
                    getCharNum = getItemAlpha(iSum)
                    getSum += getCharNum
                Next
                lenAlphaProcessItems.Add(getLen)
                sumAlphaprocessItems.Add(getSum)
                '   Console.Write(getSum & "[" & getLen & "]-")
                getSum = 0
            Next
        End Sub
        Private Shared Sub LPNProcess()
            Dim sumProcess As Integer = 0
            For index = 0 To sumAlphaprocessItems.Count - 1
                sumProcess = (sumAlphaprocessItems(index) * lenAlphaProcessItems(index)) + ((index * hash.alpha) + textPlainAscCode(index))
                hash.LPNStr &= sumProcess.ToString
                sumProcess = 0
            Next
        If hash.LPNStr.Length <= 64 Then
            'تولید هش  ادامه دهنده برای طول زیر 64 کاراکتر
            LowerLPNCipher()
        Else
            'تولید هش  ادامه دهنده برای طول بالاتر 64 کاراکتر
            UpperLPNCipher()
            End If
            '  Console.Write(hash.LPNStr)
            Return
        End Sub

        ''' <summary>
        ''' تولید هش برای ال پی ان های بیشتر از 64 کاراکتر 
        ''' </summary>
        Private Shared Sub UpperLPNCipher()
            Dim nCharList, nIdList As New ArrayList
            For index = 0 To charListCode.Count - 1
                Dim iPutten As Integer = numCharListCodeSenTwo(index) - hash.alpha
                If iPutten < 0 Then
                    iPutten += 10
                End If
                'MsgBox(charListCode(index) & vbCrLf & iPutten)
                nCharList.Add(charListCode(index))
                nIdList.Add(iPutten)
                '  Console.WriteLine(charListCode(index) & " = " & iPutten)
            Next

            Dim hashLPN As String = hash.LPNStr
            Dim allResp As Double = 0.0
            Dim sumAll As Integer = 0
            Dim lpAssign As String = String.Empty
            For index = 0 To hashLPN.Length - 1
                sumAll += (hashLPN(index).ToString * index)
            Next

            For index = 0 To hashLPN.Length - 1
                If index + 1 <> hashLPN.Length Then
                    lpAssign = hashLPN(index) & hashLPN(index + 1)
                    allResp += (lpAssign / sumAll)
                Else
                    allResp += hashLPN(index).ToString / sumAll
                End If
            Next

            ' Console.WriteLine(allResp)

            hashLPN = allResp.ToString * hash.alpha
            hashLPN = hashLPN.Replace(Chr(46), String.Empty)

            If hashLPN.StartsWith(0) Then
                hashLPN = hashLPN.Remove(0, 1)
            End If

            If hashLPN.Length >= 15 Then
                hashLPN = hashLPN.Remove(14)
            End If

            hash.LPNStr = CreateRPL(allResp.ToString.Length - 1, hashLPN)
            For index = 0 To nCharList.Count - 1
                hashLPN = hashLPN.Replace(nIdList(index), nCharList(index))
            Next

            '  Console.WriteLine(hashLPN)
            hash.result = hashLPN
            Return

        End Sub
        ''' <summary>
        ''' تولید هش برای ال پی ان های کمتر از 64 کاراکتر
        ''' </summary>
        Private Shared Sub LowerLPNCipher()
            Dim nCharList, nIdList As New ArrayList
            For index = 0 To charListCode.Count - 1
                Dim iPutten As Integer = numCharListCode(index) - hash.alpha
                If iPutten < 0 Then
                    iPutten += 10
                End If
                nCharList.Add(charListCode(index))
                nIdList.Add(iPutten)
                '  Console.WriteLine(charListCode(index) & " = " & iPutten)
            Next

        Dim hashLPN As String = hash.LPNStr
        hash.LPNStr = CreateRPL(64 - hashLPN.Length, hashLPN)


            For index = 0 To nCharList.Count - 1
                hashLPN = hashLPN.Replace(nIdList(index), nCharList(index))
            Next

            '  Console.WriteLine(hashLPN)
            hash.result = hashLPN
            Return
        End Sub
    ''' <summary>
    ''' تولید RPL از PLN
    ''' </summary>
    ''' <param name="size"></param>
    ''' <param name="hashPLN"></param>
    ''' <returns></returns>
    Private Shared Function CreateRPL(size As Int16, ByRef hashPLN As String) As String
        Dim getDevSignature As ULong = 1
        Dim result As String = String.Empty
        Dim sProcess As Long = 1
        Dim sumTextplainAsc As Long = hash.len * hash.alpha

        For index = 0 To signatureAscCode.Count - 1
            getDevSignature += signatureAscCode(index) * (index + 1)
        Next

        For index = 0 To textPlainAscCode.Count - 1
            sumTextplainAsc += textPlainAscCode(index)
        Next
        getDevSignature *= signatureAscCode.Count
        getDevSignature += sumTextplainAsc

        Dim loopIndex As Integer = 1
        While hashPLN.Length < 65
            sProcess = ((hashPLN.Length * hash.alpha) * loopIndex) * getDevSignature
            hashPLN &= sProcess.ToString
            loopIndex += 1
        End While

        hashPLN = hashPLN.Remove(64)
        Return hashPLN
    End Function
    ''' <summary>
    ''' معادله آلفا
    ''' </summary>
    Private Shared Sub AlphaProcess()
        Dim A As Decimal = 0
        Dim I = 0, resultProcess As Decimal
        Dim nRes As Boolean = Nothing
        Dim getMatProcessSingleItem As Decimal = 0
        For index = 0 To matProcessData.Count - 1
            I += 1
            getMatProcessSingleItem = matProcessData(index)
            If index = 0 Then
                resultProcess = (getMatProcessSingleItem * hash.alpha) + A
                'Console.WriteLine("(" & getMatProcessSingleItem & " * " & hash.alpha & ")   = " & resultProcess)
                alphaProcessData.Add(resultProcess)
                GetAbst(index, resultProcess, A)
            Else
                If nRes Then
                    resultProcess = (getMatProcessSingleItem * hash.alpha) - A
                    '       Console.WriteLine("(" & getMatProcessSingleItem & " * " & hash.alpha & ") -" & A & " = " & resultProcess)
                Else
                    resultProcess = (getMatProcessSingleItem * hash.alpha) + A
                    '    Console.WriteLine("(" & getMatProcessSingleItem & " * " & hash.alpha & ") +" & A & " = " & resultProcess)
                End If
                alphaProcessData.Add(resultProcess)
                nRes = GetAbst(index, resultProcess, A)
            End If
        Next
    End Sub
    ''' <summary>
    ''' تولید آلفا با محدودیت انتشار
    ''' </summary>
    ''' <param name="index"></param>
    ''' <param name="resultProcess"></param>
    ''' <param name="getA"></param>
    ''' <returns></returns>
    Private Shared Function GetAbst(index As Integer, resultProcess As Decimal, ByRef getA As Decimal)
        Dim getAStr As String = String.Empty
        Dim gSumA As Integer = 0
        If index Mod 2 <> 0 Then
            'MsgBox(resultProcess & vbCrLf & textPlainAscCode(index) & vbCrLf & hash.len)
            getAStr = (resultProcess / textPlainAscCode(index)) * hash.len
            If getAStr <= 865152361555812023 Then
                getA = getAStr
            Else
                getA = hash.len * hash.alpha * textPlainAscCode(index)
            End If

            Return True
        Else

            'برای سریز نشد در متون بالا این متغیر نیاز به خالی شدن دارد
            'ناقص است.
            getAStr = (resultProcess / textPlainAscCode(index)) * ((hash.len * (index + 1)) + textPlainAscCode(index))

            If getAStr <= 865152361555812023 Then
                getA = getAStr

            Else
                getA = hash.len * hash.alpha * textPlainAscCode(index)
            End If


            ' MsgBox("N:(" & resultProcess & " / " & textPlainAscCode(index) & ") * " & "((" & hash.len & " * " & index + 1 & ") + " & textPlainAscCode(index) & ") = " & getA)
            Return False
        End If

    End Function
    Private Shared Sub GetAscListCode(value As String, ByRef refList As ArrayList, Optional rewriteList As Boolean = False)

            If rewriteList = True And refList.Count > 0 Then
                refList.Clear()
            End If

            Dim valueLen As Integer = value.Length - 1
            Dim ascCode As Int32 = 0
            Dim getSumAll As Integer = 0
        For index = 0 To valueLen
            getSumAll += Asc(value(index))
        Next
        For index = 0 To valueLen
            'ASC + SUMALL + (Index * 4)
            ascCode = Asc(value(index)) + getSumAll + (index * 4)
            refList.Add(ascCode)
        Next
        Return
        End Sub
    ''' <summary>
    ''' مرحله 4 ام الگوریتم دی سی ال برای تاثیر و تلفیق رشته با کلید رمزنگاری
    ''' </summary>
    Private Shared Sub MatProcess()
        Dim matSum As Integer = 0
        Dim snLen As Int16 = signatureAscCode.Count - 1
        Dim txtPlainLen As Integer = textPlainAscCode.Count - 1
        Dim getAscChar As Int16 = 0
        For iMain = 0 To txtPlainLen
            getAscChar = textPlainAscCode(iMain)
            For index = 0 To snLen
                matSum += (getAscChar * signatureAscCode(index) * index)
            Next
            matProcessData.Add(matSum)
            matSum = 0
        Next
    End Sub
End Class

