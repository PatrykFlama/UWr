using System;
using System.IO;
using System.Text;
using System.IO.Compression;
using System.Security.Cryptography;

class Program
{
    static void Main()
    {
        string filePath = "file.txt";
        string encryptedFilePath = "encrypted_compressed.txt";

        // wczytaj plik do stringbuilder
        StringBuilder sb = new StringBuilder();
        using (StreamReader reader = new StreamReader(filePath))
        {
            sb.Append(reader.ReadToEnd());
        }

        // szyfrowanie AES
        string encryptedText;
        // musimy zapamiętać dane do szyfrowania, aby móc odszyfrować
        byte[] key, iv; // klucz i wektor inicjalizacyjny
        using (Aes aesAlg = Aes.Create())
        {
            key = aesAlg.Key;
            iv = aesAlg.IV;
            byte[] encryptedBytes = EncryptStringToBytes_Aes(sb.ToString(), aesAlg.Key, aesAlg.IV);
            encryptedText = Convert.ToBase64String(encryptedBytes);
        }

        Console.WriteLine("plik został zaszyfrowany");

        // kompresja GZip
        using (FileStream fs = new FileStream(encryptedFilePath, FileMode.Create))
        {
            using (GZipStream zipStream = new GZipStream(fs, CompressionMode.Compress))
            {
                using (StreamWriter writer = new StreamWriter(zipStream))
                {
                    writer.Write(encryptedText);
                }
            }
        }

        Console.WriteLine("plik został zkompresowany");


        // rekonstrukcja oryginalnego pliku
        using (FileStream fs = new FileStream(encryptedFilePath, FileMode.Open))
        {
            // dekompresja
            using (GZipStream zipStream = new GZipStream(fs, CompressionMode.Decompress))
            {
                using (StreamReader reader = new StreamReader(zipStream))
                {
                    string decompressedText = reader.ReadToEnd();

                    // deszyfrowanie
                    byte[] encryptedBytes = Convert.FromBase64String(decompressedText);
                    using (Aes aesAlg = Aes.Create())
                    {
                        aesAlg.Key = key;
                        aesAlg.IV = iv;
                        string decryptedText = DecryptStringFromBytes_Aes(encryptedBytes, aesAlg.Key, aesAlg.IV);
                        Console.WriteLine("Odczytano dane: " + decryptedText);
                    }
                }
            }
        }
    }


    static byte[] EncryptStringToBytes_Aes(string plainText, byte[] key, byte[] iv)
    {
        byte[] encrypted;
        using (Aes aesAlg = Aes.Create())
        {
            aesAlg.Key = key;
            aesAlg.IV = iv;

            ICryptoTransform encryptor = aesAlg.CreateEncryptor(aesAlg.Key, aesAlg.IV);

            using (MemoryStream msEncrypt = new MemoryStream())
            {
                using (CryptoStream csEncrypt = new CryptoStream(msEncrypt, encryptor, CryptoStreamMode.Write))
                {
                    using (StreamWriter swEncrypt = new StreamWriter(csEncrypt))
                    {
                        swEncrypt.Write(plainText);
                    }
                    encrypted = msEncrypt.ToArray();
                }
            }
        }
        return encrypted;
    }

    static string DecryptStringFromBytes_Aes(byte[] cipherText, byte[] key, byte[] iv)
    {
        string plaintext = null;
        using (Aes aesAlg = Aes.Create())
        {
            aesAlg.Key = key;
            aesAlg.IV = iv;

            ICryptoTransform decryptor = aesAlg.CreateDecryptor(aesAlg.Key, aesAlg.IV);

            using (MemoryStream msDecrypt = new MemoryStream(cipherText))
            {
                using (CryptoStream csDecrypt = new CryptoStream(msDecrypt, decryptor, CryptoStreamMode.Read))
                {
                    using (StreamReader srDecrypt = new StreamReader(csDecrypt))
                    {
                        plaintext = srDecrypt.ReadToEnd();
                    }
                }
            }
        }
        return plaintext;
    }
}
