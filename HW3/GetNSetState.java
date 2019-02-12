import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    //turn byte array into atomic integer array
    private void create_aia (byte[] v) {
        //create int array w/ size of v
        int[] ints = new int[v.length];
        
        //transfer elements of byte array to int array
        int i = 0;
        while (i < v.length) {
            ints[i] = v[i];
            i++;
        }

        //create atomic integer array with int array as argument
        value = new AtomicIntegerArray(ints);
    }

    GetNSetState(byte[] v) { create_aia(v); maxval = 127; }

    GetNSetState(byte[] v, byte m) { create_aia(v); maxval = m; }

    public int size() { return value.length(); }

    //convert atomic integer array back to byte array
    public byte[] current() {
        //create byte array w/ size of value
        byte[] v = new byte[value.length()];

        //transfer elements of atomic int array to byte array
        int i =0;
        while (i < value.length()) {
            v[i] = (byte) value.get(i); //cast to byte
            i++;
        }

        return v;
    }

    public boolean swap(int i, int j) {
    if (value.get(i) <= 0 || value.get(j) >= maxval) {
        return false;
    }
    value.decrementAndGet(i);
    value.incrementAndGet(j);
    return true;
    }    
}