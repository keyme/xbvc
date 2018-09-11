#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include <xbvc_core.h>
#include <cobs.h>
#include "gqueue.h"

#define BUFFER_SIZE 512
DECLARE_QUEUE(buf, char, BUFFER_SIZE);

extern int xbvc_encode_get_command(struct x_get_command *src, uint8_t *dest,
                                   int max_len);
extern int xbvc_decode_get_command(uint8_t *src, struct x_get_command *dest,
                                   int max_len);
extern int xbvc_encode_get_response(struct x_get_response *src, uint8_t *dest,
                                    int max_len);
extern int xbvc_decode_get_response(uint8_t *src, struct x_get_response *dest,
                                    int max_len);
extern int xbvc_encode_test_float(struct x_test_float *src, uint8_t *dest,
                                  int max_len);
extern int xbvc_decode_test_float(uint8_t *src, struct x_test_float *dest,
                                  int max_len);
bool got_heartbeat = false;
bool got_get = false;
uint8_t fluffcmp[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};

void xbvc_handle_get_command(struct x_get_command *msg)
{
    XBVCTOUCH(msg);
    printf("Got Get Command!\n");
    assert(msg->Target == 0xf00fdada);
    assert(memcmp(msg->Fluff, fluffcmp, 10) == 0);
    got_get = true;
}

void xbvc_handle_heartbeat(struct x_heartbeat *msg)
{
    XBVCTOUCH(msg);
    printf("Got heartbeat!\n");
    assert(msg->Alive == 0xdeadbeef);
    got_heartbeat = true;
}

void print_get_response(struct x_get_response *msg)
{
    printf("x_get_response:\n");
    printf("Error: 0x%x\n", msg->Error);
    printf("Target: 0x%x\n", msg->Target);
    printf("Index: %d\n", msg->Index);
    printf("Foo: 0x%x\n", msg->Foo);
    printf("Result: %d\n", msg->Result);
    printf("Bar: 0x%x\n", msg->Bar);
    printf("Version: %f\n", msg->Version);
    printf("-------------------\n");

}

void print_test_float(struct x_test_float *msg)
{
    printf("x_test_float:\n");
    printf("f1: 0x%f\n", msg->f1);
    printf("f2: 0x%f\n", msg->f2);
    printf("-------------------\n");
}

void test_encode_round_trip(void)
{
    struct x_get_response msg = {0};
    struct x_get_response decoded_msg = {0};
    uint8_t encode_buf[32] = {0};

    msg.Error = 0xdeadbeef;
    msg.Target = 0xF00;
    msg.Index = -1;
    msg.Foo = 0xa5;
    msg.Result = -8675309;
    msg.Bar = 0xff;
    msg.Version = 2.5;
    print_get_response(&msg);

    int enc_len = xbvc_encode_get_response(&msg, encode_buf,
                                           sizeof(encode_buf));
    int dec_len = xbvc_decode_get_response(encode_buf, &decoded_msg,
                                           sizeof(encode_buf));

    printf("%d | %d\n", enc_len, dec_len);

    printf("[");
    for (int i = 0; i < enc_len; i++) {
        printf("0x%x", encode_buf[i]);
        if (i < enc_len - 1) {
            printf(", ");
        }
    }
    printf("]");
    printf("\n");

    print_get_response(&decoded_msg);

    assert(memcmp(&msg, &decoded_msg, sizeof(struct x_get_response)) == 0);
}


/* This is called by the XBVC state machine */
static void loopback_init(void *params)
{
    XBVCTOUCH(params);
    queue_init(&buf, sizeof(uint8_t), BUFFER_SIZE);
}

int loopback_read(uint8_t *dst, int len)
{
    for (int i = 0; i < len; i++) {
        if(queue_is_empty(&buf)) {
            return i;
        }
        queue_dequeue(&buf, dst++);
    }
    return len;
}

int loopback_write(uint8_t *src, int len)
{
    for (int i = 0; i < len; i++) {
        queue_enqueue(&buf, src++);
    }
    return len;
}

void test_roundtrip_encoding(void)
{
    xbvc_init(NULL,
              loopback_read,
              loopback_write,
              loopback_init);

    struct x_heartbeat heartbeat;
    heartbeat.Alive = 0xdeadbeef;
    xbvc_send(&heartbeat, E_MSG_HEARTBEAT);
    for(int i = 0; i < 10000; i++) {
        xbvc_run();
        if (got_heartbeat) {
            return;
        }
    }
    assert(false);
}

void test_roundtrip_encoding2(void)
{
    xbvc_init(NULL,
              loopback_read,
              loopback_write,
              loopback_init);

    struct x_get_command getcmd;
    getcmd.Target = 0xf00fdada;
    memcpy(fluffcmp, getcmd.Fluff, 10);
    xbvc_send(&getcmd, E_MSG_GET_COMMAND);
    for(int i = 0; i < 10000; i++) {
        xbvc_run();
        if (got_get) {
            return;
        }
    }
    assert(false);
}

void test_id_integrity(void)
{
    assert(E_MSG_PING == 0xdeadbeef);
    printf("ID Integrity test passed\n");
}


void test_double_float()
{
    struct x_test_float msg = {0};
    struct x_test_float decoded_msg = {0};
    uint8_t encode_buf[32] = {0};

    msg.f1 = 2.5;
    msg.f2 = 3.5;
    print_test_float(&msg);

    int enc_len = xbvc_encode_test_float(&msg, encode_buf,
                                         sizeof(encode_buf));
    int dec_len = xbvc_decode_test_float(encode_buf, &decoded_msg,
                                         sizeof(encode_buf));

    printf("%d | %d\n", enc_len, dec_len);

    printf("[");
    for (int i = 0; i < enc_len; i++) {
        printf("0x%x", encode_buf[i]);
        if (i < enc_len - 1) {
            printf(", ");
        }
    }
    printf("]");
    printf("\n");

    print_test_float(&decoded_msg);

    assert(memcmp(&msg, &decoded_msg, sizeof(struct x_test_float)) == 0);
    
}


int main()
{
    test_encode_round_trip();
    test_roundtrip_encoding();
    test_roundtrip_encoding2();
    test_id_integrity();
    test_double_float();
    return 0;
}
