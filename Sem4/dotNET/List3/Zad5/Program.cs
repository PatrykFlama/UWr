public class ListHelper
{
    public static List<TOutput> ConvertAll<T, TOutput>(
        List<T> list,
        Converter<T, TOutput> converter)
    {
        if (list == null)
            throw new ArgumentNullException(nameof(list));
        if (converter == null)
            throw new ArgumentNullException(nameof(converter));

        List<TOutput> result = new List<TOutput>(list.Count);
        foreach (T item in list)
        {
            result.Add(converter(item));
        }
        return result;
    }

    public static List<T> FindAll<T>(
        List<T> list,
        Predicate<T> match)
    {
        if (list == null)
            throw new ArgumentNullException(nameof(list));
        if (match == null)
            throw new ArgumentNullException(nameof(match));

        List<T> result = new List<T>();
        foreach (T item in list)
        {
            if (match(item))
                result.Add(item);
        }
        return result;
    }

    public static void ForEach<T>(
        List<T> list,
        Action<T> action)
    {
        if (list == null)
            throw new ArgumentNullException(nameof(list));
        if (action == null)
            throw new ArgumentNullException(nameof(action));

        foreach (T item in list)
        {
            action(item);
        }
    }

    public static int RemoveAll<T>(
        List<T> list,
        Predicate<T> match)
    {
        if (list == null)
            throw new ArgumentNullException(nameof(list));
        if (match == null)
            throw new ArgumentNullException(nameof(match));

        int index = 0;
        while (index < list.Count)
        {
            if (match(list[index]))
                list.RemoveAt(index);
            else
                index++;
        }
        return list.Count;
    }

    public static void Sort<T>(
        List<T> list,
        Comparison<T> comparison)
    {
        if (list == null)
            throw new ArgumentNullException(nameof(list));
        if (comparison == null)
            throw new ArgumentNullException(nameof(comparison));

        // bubble sort
        bool swapped = true;
        while (swapped)
        {
            swapped = false;
            for (int i = 0; i < list.Count - 1; i++)
            {
                if (comparison(list[i], list[i + 1]) > 0)
                {
                    T temp = list[i];
                    list[i] = list[i + 1];
                    list[i + 1] = temp;
                    swapped = true;
                }
            }
        }
    }
}
